-module(akm_mailer).

-include("akm.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(TEMPLATE_FILE, "request_revoke.dtl").

-export([send_revoke_mail/4]).

-spec send_revoke_mail(string(), binary(), binary(), binary()) ->
    ok | {error, {failed_to_send, term()}}.
send_revoke_mail(Email, PartyID, ApiKeyID, Token) ->
    Mod = ?RENDER_MODULE,
    {ok, Body} = Mod:render([
        {url, url()},
        {party_id, PartyID},
        {api_key_id, ApiKeyID},
        {revoke_token, Token}
    ]),
    BinaryBody = unicode:characters_to_binary(Body),
    logger:info("Try send email with body: ~p", [BinaryBody]),
    Pid = self(),
    case
        gen_smtp_client:send(
            {from_email(), [Email], BinaryBody},
            [
                {ssl, true},
                {relay, relay()},
                {port, port()},
                {username, username()},
                {password, password()}
            ],
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

timeout() ->
    maps:get(timeout, get_env(), 3000).

port() ->
    #{port := Port} = get_env(),
    to_int(Port).

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
    Timeout = timeout(),
    receive
        {sending_result, {ok, _Receipt}} ->
            ok;
        {sending_result, Error} ->
            {error, Error}
    after Timeout ->
        {error, {failed_to_send, sending_email_timeout}}
    end.

to_int(Value) when is_integer(Value) -> Value;
to_int(Value) when is_binary(Value) -> erlang:binary_to_integer(Value);
to_int(Value) when is_list(Value) -> erlang:list_to_integer(Value).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec to_int_test() -> _.
to_int_test() ->
    ?assertEqual(123, to_int(123)),
    ?assertEqual(123, to_int(<<"123">>)),
    ?assertEqual(123, to_int("123")).

-spec wait_test() -> _.
wait_test() ->
    erlang:send_after(timeout() + 10, self(), timeout),
    ?assertEqual({error, {failed_to_send, sending_email_timeout}}, wait_result()).

-endif.