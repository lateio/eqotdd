eqotdd
=====

Purpose
----

Implements Quote of the Day protocol described in RFC 865 (https://tools.ietf.org/rfc/rfc865.txt)


Want to help?
----

There can never be too many Quotes. Additionally, not all Quotes were uttered equal.


Behavior
----

eqotdd (Erlang Quote of the Day Daemon) listens on configured TCP, UDP and/or TLS ports and when a connection or a datagram is received, a short string of characters — the Quote — is sent back to the peer as a response. Any bytes sent by the peer are discarded.

The returned string is read from user configured/supplied plain text file which adheres to a simple syntax. By default the Quote is changed daily to a random new one.


Configuration
----

eqotdd can be configured with Erlang application configuration parameters.

```Erlang
{'eqotdd', [
    {'new_quote_at', calendar:time() | {'interval', {non_neg_integer(), non_neg_integer(), non_neg_integer()}}},
    {'quote_order', 'random' | 'in_order'},
    {'quotes_source', module()},
    {'quotes_file', {'priv_file', App :: atom(), Path :: string()} | {'file', Path :: string()}},
    {'max_quote_length', pos_integer()},
    {'line_break', [char()] | char()},
    {'append_line_break', boolean()},
    {'address', [inet:ip_address(), ...]},
    {'udp', boolean()},
    {'udp_port', inet:port_number()},
    {'udp_address', [inet:ip_address(), ...]},
    {'tcp', boolean()},
    {'tcp_port', inet:port_number()},
    {'tcp_address', [inet:ip_address(), ...]},
    {'tls', boolean()},
    {'tls_port', inet:port_number()},
    {'tls_address', [inet:ip_address(), ...]},
    {'tls_certfile', Path :: string()},
    {'tls_keyfile',  Path :: string()}
]}.
```

* `'new_quote_at'` determines when the Quote is changed.
  If `calendar:time()` is used, the Quote changes when localtime equals the specified time.
  If `{'interval', {1,10,15}}` is used, the Quote changes every 1 hour, 10 minutes and 15 seconds.
  Default is `{24,0,0}`
* `'quote_order'` determines how the next Quote is chosen. Default is `'random'`
* `'quotes_source'` specifies the source for Quotes.
  Custom sources can be created by implementing `'eqotdd_quotes_source'` behavior.
  Default is `'eqotdd_file'`
* `'quotes_file'` specifies the file from which Quotes are read.
  `{'priv_file', App :: atom(), string()}` uses a file in the given apps priv directory.
  `{'file', string()}` specifies an arbitrary file.
  Default is `{'priv_dir', 'eqotdd', "quotes"}`
* `'max_quote_length'` will cause any Quote which exceeds the specified limit to be silently discarded.
  Note that this limit applies to bytes, not characters. Thus with Quote limit set to 4, a Quote made by repeating 'a' character could be 4 characters long, while a Quote made by repeating 'ä' character could only be 2 characters long (assuming UTF-8).
  Default is `511`
* `'line_break'` specifies the line break to use in quotes spanning multiple lines and when appending a line break to the returned Quote. Default is `[$\r,$\n]`
* `'append_line_break'` determines if a trailing line break is added to the returned Quote. Default is `'true'`
* `'address'`, `'udp_address'`, `'tcp_address'` and `'tls_address'` control to which address each of the available transports are bound to. When specified, transport specific addresses will be preferred to the generic ones.
  Ie: UDP sockets will be preferentially bound to addresses in `'udp_address'`. Only if none were specified will addresses in `'address'` be tried. If no addresses are specified, transports will bind to wildcard addresses on usable/configured interfaces.
  By default no addresses are specified.
* `'udp'`, `'tcp'` and `'tls'` specify if the transport is to be enabled.
  By default UDP and TCP are enabled and TLS is not enabled.
* `'udp_port'`, `'tcp_port'`, `'tls_port'` specify which ports to use for each transport.
  By default UDP and TCP both use `8017`. Default for TLS in `8018`
* `'tls_certfile'` specifies the certificate file to be used with TLS. By default this has no value.
* `'tls_keyfile'` specifies the private key file to be used with TLS. By default this has no value.

It is *not* possible to configure eqotdd to return a different, random Quote for every request.


Quotes File
----

Syntax of the Quotes file is minimal
```
% This is a comment
    % This is another comment

This is a quote
This is another quote
    split on two lines

This is a third quote.
    % Just a comment

    It contains a line break.
\% This is the last quote
    \% with a few escaped and regular percent signs %%
```

The previous example defines the following four Quotes:
1. This is a quote
2. This is another quote split on two lines
3. This is a third quote.
   It contains a line break.
4. % This is the last quote % with a few escaped percent signs %%

In the simplest case, a Quote is just a single line of text. First character of a Quote line must not be whitespace (space or tab) or the percent sign. All trailing whitespace is discarded.

It is possible for Quotes to be split over multiple lines. When a line after a normal Quote line starts with whitespace and is not a comment, all whitespace will be collapsed into a single space character. Then the space and the new text line will be included in the previously started Quote.

It is possible for Quotes to contain line breaks. When a Quote is split over multiple lines and between these split lines are one or more empty, non-comment lines, these empty lines are collapsed into a single line break and the lines containing text are combined with the line break in between them.

`file:read_line/1` is used for getting lines from the Quote file. Thus `$\n` and `[$\r,$\n]` are treated as line breaks when the file is parsed. Before parsed Quotes are returned, any line breaks are replaced with the user configurable line break value.

Quotes file is assumed to contain text made up of 8-bit characters. Thus it can contain for example ASCII, Extended-ASCII or UTF-8. Whether the client can display the mess we sent them is, obviously, another matter entirely.


Ports
----

According to RFC 865, port number 17 is to be used for both TCP and UDP. However, binding these ports requires that the user is privileged. As little code should generally be run as a privileged user and because eqotdd at no other point needs any special privileges to work correctly, it is recommended to either redirect the ports using a firewall or make use of capabilities on Linux. eqotdd uses ports `8017` and `8018` as the defaults in part to encourage the firewall redirect option.

The spec defines no standard port for qotd over TLS (qotds) and thus the user can freely choose an appropriate one.


Missing Features
----

Although it would obviously be desirable, eqotdd is not currently capable of requiring TLS clients to present a valid client certificate before the Quote is returned.


Build
-----

    $ rebar3 compile
