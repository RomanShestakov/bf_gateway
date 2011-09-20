betfairgateway - Erlang based implementation of betfair API
==============================================

## Dependencies

1. Erlsom for XML parsing
2. Detergent for SOAP interactions
3. log4erl for logging

## Building

betfairgateway uses rebar for building and wraps it in a Makefile for convenience.

First clone from GitHub:

    $ git clone git://github.com/romanshestakov/betfairgateway.git

Then change into the newly created directory:

    $ cd betfairgateway

And make:

    $ make

Rebar will first pull in dependencies from GitHub, attempt to build them all, then build betfairgateway.

## Testing

License
=======
