# Emacs


# Install

Download Emacs tarball from `https://ftp.gnu.org/gnu/emacs/`

Extract with
```
tar zxvf emacs-<version>.tar.gz
```

Set up install location, the default is `/usr/local/`
```
./configure --prefix=/usr/local
```

Then install with
```
make && make install
```

## Install errors

Gnutls not found
```
apt-cache search libgnutls.*-dev
apt-get install libgnutls.<version>-dev
```

In case your Emacs icon is missing
```
sudo gtk-update-icon-cache /usr/share/icons/hicolor
```



# Basics

Emacs looks for an init file `~/.emacs`, `~/.emacs.el` and `~/.emacs.d/init.el` in that order.


## Notation

Mnemonics - makes sense but kinda horrible

For example, the combination `C-n` (hold Control and press n) moves cursor to the next line and `C-p` to the previous line.

- `C` stand for Control
- `M` (meta) stands for Alt
- `S` for Shift
- `s` for Super or windows

You can also have combinations such as `C-M-x` (hold Control and Alt, then press x). 

Note that Emacs will try to map shifted keys to regular keys if they are not bound, so for example `M-x` and `M-X` map to the same thing, i.e. `execute-extended-command`.

`C-x C-f` is two combinations is sequence, so press `C-x` first (you will see it minibuffer as `C-x-`) and then press `C-f`. 
**DO NOT** try to hold Control while trying to press `x` and `f` in sequence because it puts too much strain on your fingers and can result in RSI (Repetitive-Strain-Injury) Syndrom. Or even better is to remap frequent keys into dedicated keys or use fewer key strokes.

## Movement

- `C-n` move down a line
- `C-p` move up a line
- `C-f` move forward a character
- `C-b` move backward a character
- `M-f` move forward a word (alphanumeric sequence)
- `M-b` move backward a word
- `C-x C-f` find file and open
- `C-x d` dired or show directory
- `C-x b` show buffers which are usually currently opened files
- `C-x C-c` exit emacs
- `C-x C-s` save file

- `C-a` move beginning of line
- `C-e` move end of line
- `C-s` search forward, keep pressing `C-s` to go to next match
  - you can either press `<enter>` to stay on highlighted match or press `C-g` to back to original position
- `C-r` search backward, same as above but use `C-r` to go previous match
- `C-w` cut or in Emacs lingo "kill region"
- `M-w` copy to kill ring
- `C-y` paste from kill ring
  - `M-y` cycle paste from kill ring
- `M-x` execute command
  - search for either dumb terminal `shell` or `term` which behaves same as regular terminal
  - Note: inside `term`, the prefix command `C-x` switches to `C-c`, so to send an actual `C-c` to the terminal type it twice, i.e. `C-c C-c`.


## Emacs vs Vim

`vi` is archaic version of Vim. With Vim you usually edit files and go back to terminal but with Emacs you try to do everything inside it.

There is a compromise called `evil-mode` which let's you use Vim keybinding.

## key bindings
-
`C`
`C-h c` describe key

## Files or Dired

### abbrev(iation)

```
(add-to-list
    'directory-abbrev-alist
    '("^/jou" . "~/mnt/mdbackup/journal" ))
```
Then type `C-x C-f /jou <RET>`

### Bookmark files
`C-x r m` mark for bookmark
`C-x r b` prompt for saved bookmarks


### ssh

```
# ~/.ssh/config

Host goo
     HostName google.ca
     User root
``` 
Then type `C-x C-f /ssh:goo:/` to get password prompt 

#### sudo or TRAMP 

Type `C-x C-f /sudo:user@localhost:`



# Troubleshooting

## Cannot install packages

if your package expires either disable signature checking
```
(setq package-check-signature nil)
```

or install it manually
```
gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40
```

*source*: http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html


