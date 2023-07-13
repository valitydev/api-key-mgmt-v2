-module(akm_mailer).

-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([send_revoke_mail/4]).

%% Suppress akm_mail_request_revoke:render/1 being unknown, as there's no erlang file generated
-dialyzer({[no_unknown], [send_revoke_mail/4]}).

-spec send_revoke_mail(string(), binary(), binary(), binary()) ->
    ok | {error, {failed_to_send, term()}}.
send_revoke_mail(Email, PartyID, ApiKeyID, Token) ->
    {ok, Body} = akm_mail_request_revoke:render([
        {url, url()},
        {party_id, PartyID},
        {api_key_id, ApiKeyID},
        {revoke_token, Token}
    ]),
    BinaryBody = erlang:iolist_to_binary(Body),
    case
        gen_smtp_client:send(
            {from_email(), [Email], BinaryBody},
            [{relay, relay()}, {username, username()}, {password, password()}]
        )
    of
        {error, Reason} ->
            {error, {failed_to_send, Reason}};
        _Receipt ->
            ok
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

get_env() ->
    genlib_app:env(akm, mailer, #{
        url => "vality.dev",
        from_email => "example@example.com",
        relay => "smtp.gmail.com",
        username => "username",
        password => "password"
    }).
