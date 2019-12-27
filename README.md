# Emacs


# Install

Download Emacs tarball from https://ftp.gnu.org/gnu/emacs/

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

## Using PPA (Personal Package Archive)

``` bash
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt install emacs26
```

Then delete the repository from `/etc/apt/sources.list.d`, if you do not trust it.

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

## Emacs vs Vim

`vi` is archaic version of Vim. With Vim you usually edit files and go back to terminal but with Emacs you try to do everything inside it.

There is a compromise called `evil-mode` which let's you use Vim keybinding.


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

### Grammar

```EBNF
<key-sequence> 		:== <key-combination>+
<key-combination> 	:== <modifier> { '-' <modifier> } <key>
<modifier> 			:== [CSsM]
<key> 				:== [a-zA-Z] | <special-key>
<special-key> 		:== '<return>' | 'RET' | ...
```
e.g. `C-n`, `C-x C-f`, `<return>`, `RET`, etc.

## Movement

cheat sheet at https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf

- `C-n` move down a line
- `C-p` move up a line
- `C-f` move forward a character
- `C-b` move backward a character
- `C-x C-f` find file and open
- `C-x d` dired or show directory
- `C-x b` switch buffer in minibuffer
- `C-x C-b` show buffers
- `C-x C-c` exit emacs
- `C-x C-s` save file
- `M-x` execute command


## Learn Emacs using Emacs

- `C-h k` describe key pressed
- `C-h c` describe key briefly
- `C-h f` describe function name
- `C-h v` describe variable name
- `C-h m` describe current modes used

Modes to help
- `(which-key)` show commands for a prefix after a delay


## Command line interface

Type either `shell` for dumb terminal or `term` which behaves same as regular terminal
**Note**: inside `term`, the prefix command `C-x` switches to `C-c`, so to send an actual `C-c` to the terminal type it twice, i.e. `C-c C-c`.

Shell mode commands
- `M-n` and `M-p` to cycle next and previous commands
- `C-a` and `C-e` go to beggining and end of line
- `M-x send-invisible` to hide typed text

Anywhere in Emacs
- `M-!` to execute single shell command
- `C-u M-!` same as `M-!` but insert command output at the mark point

## Editing files

`M-x align-regexp` to align columns based on a regex separator

### Create shortcut
```
(add-to-list
    'directory-abbrev-alist
    '("^/jou" . "~/mnt/mdbackup/journal" ))
```
Then type `C-x C-f /jou <RET>`

### Bookmark files
`C-x r m` mark for bookmark
`C-x r b` prompt for saved bookmarks

## Dired (Directory Editor)

Type `C-x d` to open `dired`

- `d` mark for deletion
- `x` delete all `d`-marked files and folders
- `m` mark for copy, move
- `u` unmark file or folder
- `U` unmark all
- `C` copy a file and prompt for new file name
- `R` rename a file or folder
- `+` create folder
- `Z` compress files using `gzip`; note the old file is renamed to `*.gz`
- `g` revert buffer
- `<` previous directory in a current dired buffer
- `>` next directory in dired buffer
- `^` go to parent directory



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



# ICICLES

Download required libraries

```
wget https://www.emacswiki.org/emacs/download/icicles{,-chg,-cmd1,-cmd2,-doc1,-doc2,-face,-fn,-mac,-mcmd,-mode,-opt,-var}.el
```

```
https://www.emacswiki.org/emacs/download/bookmark%2b-mac.el
```


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


