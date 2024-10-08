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

## MacOS install

After installing `brew`,

```
brew install --cask emacs
```

Go to `Finder > Applications`, then right-click `Open`. Otherwise, you get [Apple can't check app for malicious software](https://support.apple.com/en-ca/guide/mac-help/mchleab3a043/mac)

If you get the error [`Opening directory: Operation not permitted, /Users/nalma/Documents/`](https://apple.stackexchange.com/a/371945/301364), then give permission to `ruby` which launches the Emacs.


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

e.g. `C-n`, `C-x C-f`, `<return>`, `RET`, etc.

You can also have combinations such as `C-M-x` (hold Control and Alt, then press x). 

Note that Emacs will try to map shifted keys to regular keys if they are not bound, so for example `M-x` and `M-X` map to the same thing, i.e. `execute-extended-command`.

`C-x C-f` is two combinations is sequence, so press `C-x` first (you will see it minibuffer as `C-x-`) and then press `C-f`. 
**DO NOT** try to hold Control while trying to press `x` and `f` in sequence because it puts too much strain on your fingers and can result in RSI (Repetitive-Strain-Injury) Syndrom. Or even better is to remap frequent keys into dedicated keys or use fewer key strokes.



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
**Note**: 
- inside `term`, the prefix command `C-x` switches to `C-c`, so to send an actual `C-c` to the terminal type it twice, i.e. `C-c C-c`.
- In `term-mode`, command `C-c C-j` switches to `term-line-mode` and you can navigate text with regular key-binds; command `C-c C-k` switches to `term-char-mode` which behaves like terminal emulator.
- In order to send escaped characters to `term`, switch to `term-line-mode` with `C-c C-j` then inserted quoted character `C-q C-x` and switch back to `term-char-mode`. So now, `C-x` is sent to the terminal emulator.

Shell mode commands
- `M-n` (or `C-<up>`) and `M-p` (or `C-<down>`) to cycle next and previous commands
- `C-a` and `C-e` go to beggining and end of line
- `M-x send-invisible` to hide typed text
- `M-r` (`comint-history-isearch-backward-regexp`; similar to regular `C-r` in terminal emulators) search from previous commands
  - After `M-r`, cycle matches with `C-r` backward or `C-s` forward in history
- `C-c M-r` (`comint-previous-matching-input-from-input`) search backwards using current prompt string
- `C-c C-l` create buffer with command per line

> Often it is useful to reexecute several successive shell commands that were previously executed in sequence. To do this, first find and reexecute the first command of the sequence. Then type `C-c C-x`; that will fetch the following command—the one that follows the command you just repeated. Then type `<RET>` to reexecute this command. You can reexecute several successive commands by typing `C-c C-x <RET>` over and over.

_source_: https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell-Ring.html

Anywhere in Emacs
- `M-!` to execute single shell command
- `C-u M-!` same as `M-!` but insert command output at the mark point

### Copy and Paste

Usually `C-w`, `M-w` and `C-y` are used for cutting, copying and pasting. Inside a terminal use `S-<insert>` and `C-<inseart>` for copying and pasting; the mark can be set with `C-<space>` albeit being invisible.

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


## Files

Open `find-file` by pressing `C-x C-f`

### sudo or TRAMP 

Use`C-x C-f /sudo:user@localhost:`, this is similar to `sudo -s` function.
To mimic `su` issue command and type  `C-x C-f /su:root@localhost:/`; localhost is synonymous with hostname.

### SSH

Syntax `/<method>:<user>@<host>:<dir>`.

For Linux-based systems, `/ssh:user@host:/`

On MS Windows, use PuTTY's `plink` method, `/plink:user@host:/path/to/file`


#### Multihop

Incase you need to change user inside remote host then issue `C-x C-f /ssh:userA@remoteA|ssh:userB@remoteA:/`. The same method can be used to hop from a remote host to another remote host.

#### Direct shell 
`default-directory` variable dictaces where shell is opened. The following function will prompt for SSH string, e.g. "/ssh:user@host:/".

```
(defun remote-shell (remote-string)
  (interactive "sRemote:")
  (let ((default-directory remote-string))
    (shell remote-string)))
```

##### Config

```
# ~/.ssh/config

Host goo
     HostName google.ca
     User root
``` 
Then type `C-x C-f /ssh:goo:/` to get password prompt 


### Log files

Type `M-x` and then
- `revert-buffer` to refresh the contents of current buffer from hard storage
- `auto-revert-mode` minor mode for automatically reverting a buffer
- `auto-revert-tail-mode` minor mode for reverting and moving point to end of file

### Editing

`M-x align-regexp` to align columns based on a regex separator

#### Writing unicode characters

To type unicode characters, the toggle custom input `C-\` and type `\lambda` to get `λ`.

To find how a unicode can be inserted, hover over it and type `C-u C-x =`.

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


# MODE LINE - Emacs

## Default

```
cs:ch-fr buffer pos line (major minor)
```

`cs` describes character set
`ch` 
1. `--` unmodified both locally and on disk
2.  `**` buffer is modified
3.  `%*` READ-ONLY file changed on disk
4.  `%%` READ-ONLY file not changed

# ELISP

List is `'(a f b)`, without `'` an apostrophe the first atom (indivisible unit in elisp) is treated as a function.

# Frequent key-bindings

## Arrow keys

Universal across many applications. Having ease access of modifier keys is important.


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


