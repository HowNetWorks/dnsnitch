# dnsnitch

## Build & Run

1. Install https://www.haskellstack.org/
1. Build with `stack build`
1. Run tests with `stack test`
1. Run server with `stack exec dnsnitch 1053 8080`

## Test it

1. export DNSNITCHKEY=$(dd if=/dev/urandom count=512 2>/dev/null| openssl dgst -sha224)
1. dig +short -p 1053 $DNSNITCHKEY.dnstest.example.com @127.0.0.1
1. curl -o- http://127.0.0.1:8080/$DNSNITCHKEY

## example

```console
$ export DNSNITCHKEY=$(dd if=/dev/urandom count=512 2>/dev/null| openssl dgst -sha224)
$ dig +short -p 1053 $DNSNITCHKEY.dnstest.example.com @127.0.0.1
127-0-0-1.dnsresult.example.com.
$ curl -o- http://127.0.0.1:8080/$DNSNITCHKEY
720ed329b96b23ea91beb29d07e0415453358468f7b6f277722dfd44 127-0-0-1
```
