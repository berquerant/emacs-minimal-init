# Minimal init

My minimal configuration of Emacs.

# Usage

``` emacs-lisp
(require 'minimal-init)
(minimal-init-setup)
```

# Init

## debian

``` shell
curl -LO "https://raw.githubusercontent.com/berquerant/emacs-minimal-init/main/bin/init-debian.sh"
chmod +x ./init-debian.sh
EMACSD="${HOME}/.emacs.d" ./init-debian.sh
```
