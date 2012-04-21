bf_gateway - Erlang based implementation of betfair API
==============================================

allows to subsribe to the betfair markets, pulls prices and pushes them to 0MQ bus so client application could subsribe to the events published thru the message bus.

## Dependencies

1. Erlsom for XML parsing
2. Detergent for SOAP interactions
3. log4erl for logging
4. mochiweb for json parsing
5. erlzmq - erlang bindings for 0MQ
6. zeromq - 0MQ 

## Building

bf_gateway uses rebar for building and wraps it in a Makefile for convenience.
0MQ - is c based, you might need to install the following packages: pkg-config, autoconf, automake and libtool (if you use MacOX, I would recommend to use macports). Otherwise you might get errors during build.
more details could be found here:
http://lists.zeromq.org/pipermail/zeromq-dev/2010-February/002235.html


First clone from GitHub:

    $ git clone git://github.com/romanshestakov/bf_gateway.git

Then change into the newly created directory:

    $ cd bf_gateway

Add your betfair username and password into rel/files/app.config
do the same in src/bf_gateway.app.src

then build release:

    $ make rel

Rebar will first pull in dependencies from GitHub, attempt to build them all, then build bf_gateway.

start app with 
./rel/bf_gateway/bin/bf_gateway console

if everything ok, you should see 
"succesfully logged to betfair"

after that you can run commands specified by betfair API
e.g.
bf_gateway:getMarketInfo(102873781).



## this is not needed just for a record how to generate records from wsdl file
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

make test

License
=======
