betfairgateway - Erlang based implementation of betfair API
==============================================

eventually, this application should be able to accept subsribe requests and pull data for required markets and publish prices in json format to zeromq.


## Dependencies

1. Erlsom for XML parsing (there are couple of problems with the original version of erlsom, raised issues to the owner of project. for now need to use fork in git@github.com:RomanShestakov/erlsom.git)
2. Detergent for SOAP interactions
3. log4erl for logging

## Building

betfairgateway uses rebar for building and wraps it in a Makefile for convenience.

First clone from GitHub:

    $ git clone git://github.com/romanshestakov/betfairgateway.git

Then change into the newly created directory:

    $ cd betfairgateway

Add your betfair username and password into rel/files/app.config
do the same in src/betfairgateway.app.src

then build release:

    $ make rel

Rebar will first pull in dependencies from GitHub, attempt to build them all, then build betfairgateway.

start app with 
./rel/betfairgateway/bin/betfairgateway console

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

# because of some issues in the original version of erlsom need to edit erlsom_parse.erl, comment out
%% case TypeDef#type.nillable of
%% 	    true ->
%% 	      ok;
%% 	    Err1 ->
%%               throw({error, "Unexpected attribute: " ++ LocalName, TypeDef, Err1})
%% 	  end;


## Testing

make test


License
=======
