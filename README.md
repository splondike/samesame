A little spartan at the moment, but this program lets you verify that your internet connection isn't being MiTM-ed when you accept an SSH, or SSL certificate. It also lets you check the hash of a URL. Use it like this:

```shell
$ tor -f torrc # Run this in a separate terminal
```

```shell
$ ./samesame ssh stefansk.name # This means you're not being attacked, and can add the known\_host
OK
17:C7:F6:DA:8D:1B:F7:33:6C:1D:7B:EA:53:F4:34:D2:30:35:64:84
```

```shell
$ ./samesame ssh stefansk.name # This means you're being MiTM-ed
FAIL
Tor 1: 2048 d9:21:a6:1d:61:7d:3d:4f:fb:03:81:34:c0:04:b2:aa stefansk.name (RSA)
Tor 2: 2048 d9:21:a6:1d:61:7d:3d:4f:fb:03:81:34:c0:04:b2:aa stefansk.name (RSA)
Local: 2048 00:00:00:00:00:00:00:00:00:00:00:00:00:00:00:00 stefansk.name (RSA)
```

## Building

Just use GHC (the Haskell compiler)

```shell
ghc -o samesame samesame.hs
```
