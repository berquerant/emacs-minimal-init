# emacs-minimal-init

My minimal configuration of Emacs.

- Modifies built-in package settings.
- No changes to keybindings.
- No third-party packages installed.

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
