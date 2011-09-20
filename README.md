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


## how to generate records from hrl file

1. save BFExchangeService.wsdl and BFGlobalService.wsdl from betfair website to priv
2. comment out  <xsd:import namespace="http://schemas.xmlsoap.org/soap/encoding/"/> from wsdl files, as erlsom can't parse this 
namespace
3. init models:
Wsdl = detergent:initModel("file://../priv/BFGlobalService.wsdl").
Wsdl2 = detergent:initModel("file://../priv/BFExchangeService.wsdl").
4. generate hrl records
#detergent:write_hrl(Wsdl, "BFGlobalService.hrl").
#detergent:write_hrl(Wsdl2, "BFExchangeService.hrl").

detergent:write_hrl("file://./priv/BFGlobalService.wsdl", "BFGlobalService.hrl", "BFG").
detergent:write_hrl("file://./priv/BFExchangeService.wsdl", "BFExchangeService.hrl", "BFE").

## Testing

License
=======
