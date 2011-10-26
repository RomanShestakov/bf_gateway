#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name bf_gateway@127.0.0.1 \
    -setcookie rs -s bf_gateway_app start
