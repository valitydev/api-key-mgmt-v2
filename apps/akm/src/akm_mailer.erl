-module(akm_mailer).

-include("akm.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(TEMPLATE_FILE, "request_revoke.dtl").

-export([send_revoke_mail/4]).

-spec send_revoke_mail(string(), binary(), binary(), binary()) ->
    ok | {error, {failed_to_send, term()}}.
send_revoke_mail(_Email, PartyID, ApiKeyID, Token) ->
    Mod = ?RENDER_MODULE,
    {ok, Body} = Mod:render([
        {url, url()},
        {party_id, PartyID},
        {api_key_id, ApiKeyID},
        {revoke_token, Token}
    ]),
    BinaryBody = erlang:iolist_to_binary(Body),
    Pid = self(),
    case
        gen_smtp_client:send(
            %            {from_email(), [Email], BinaryBody},
            {from_email(), ["a.losev@empayre.com"], BinaryBody},
            [{relay, relay()}, {username, username()}, {password, password()}],
            fun(Result) -> erlang:send(Pid, {sending_result, Result}) end
        )
    of
        {error, Reason} ->
            {error, {failed_to_send, Reason}};
        {ok, _SenderPid} ->
            wait_result()
    end.

url() ->
    #{url := URL} = get_env(),
    URL.

from_email() ->
    #{from_email := From} = get_env(),
    From.

relay() ->
    #{relay := Relay} = get_env(),
    Relay.

username() ->
    #{username := Username} = get_env(),
    Username.

password() ->
    #{password := Password} = get_env(),
    Password.

%port() ->
%    #{port := Port} = get_env(),
%    Port.

get_env() ->
    genlib_app:env(akm, mailer, #{
        url => "https://vality.dev",
        port => 465,
        from_email => "example@example.com",
        relay => "smtp.gmail.com",
        username => "username",
        %% NOTICE: for gmail need to generate password for application in https://myaccount.google.com/apppasswords
        password => "password"
    }).

wait_result() ->
    receive
        {sending_result, {ok, _Receipt}} ->
            ok;
        {sending_result, Error} ->
            {error, Error}
    after 3000 ->
        {error, {failed_to_send, sending_email_timeout}}
    end.
