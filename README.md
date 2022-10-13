This document describes the various components of our LSP  implementation.

# %argo
The desk containing the implementation for the `language-server` agent. 

Install the `argo` desk from `~dister-dozzod-middev` - `|install ~dister-dozzod-middev %argo`. After installation the `%language-server` agent should be running. If you want to run this on a fake ship, copy from the github repo or install on live ship and then mount and copy the desk folder to your fake pier.

# hoon-language-server

Earth-side component of a language server for Hoon. Enables two-way communication between supported text editors and the `language-server` agent on an urbit ship.

[github](https://github.com/urbit/hoon-language-server/)

## Installation

### npm

```
npm install -g @urbit/hoon-language-server
```

### nix (from the repo)

```
nix-build -E 'with import <nixpkgs> {}; callPackage ./default.nix {}'
nix-env -i ./result
```

## Running

> **_NOTE:_**  Usually your editor will handle this, you just need to set the correct parameters in your editor configuration.

`hoon-language-server -p 80 -d 0 -u http://localhost -s zod -c lidlut-tabwed-pillex-ridrup`

### Configuration

- `-p`: HTTP port of your (running) ship
- `-d`: `didSave` event delay
- `-u`: ship url
- `-s`: `@p` of ship (without a sig)
- `-c`: `+code` of ship (without a sig)

# Editor Setup

Your code editor now needs to use `hoon-language-server` as an LSP provider. Supported plugins:
## VSCode

- [hoon-vscode](https://github.com/famousj/hoon-vscode)
- [hoon-assist-vscode](https://github.com/urbit/hoon-assist-vscode)

#### Configuration

Configuration is done in VSCode's extension settings. The following needs to be provided in order to correctly launch the language server and connect to a running urbit ship:

- @p
- HTTP port (the extension assumes the ship is running locally)
- code (retrieved by running `+code` in your ship's dojo)

The extension will attempt to run `hoon-language-server` with the above arguments, so make sure it's installed and discoverable by VSCode. 


## Emacs

- [hoon-mode.el](https://github.com/urbit/hoon-mode.el)

## Vim

- [hoon.vim](https://github.com/urbit/hoon.vim)

`hoon.vim` does not use the language server itself, but the github page describes a setup using [vim-lsp](https://github.com/prabirshrestha/vim-lsp).

## Neovim

Neovim users should use [hoon.vim](https://github.com/urbit/hoon.vim) with one of the following LSP setups:

### Native LSP

Install [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig).  To use the [default configuration](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.txt#hoon_ls), add the following to `init.lua`:

```
require'lspconfig'.hoon_ls.setup{}
```

To modify the default options use:

```
require'lspconfig'.hoon_ls.setup{
  cmd = {"hoon-language-server", "-p", "8080"}
}
```

You can include lua snippets in your `init.vim` like so:

```
lua << EOF
require'lspconfig'.hoon_ls.setup{}
EOF
```

### coc.nvim

Install and configure [coc.nvim](https://github.com/neoclide/coc.nvim), then add a `languageserver` entry to `~/.config/nvim/coc-settings.json`:

```
{
  "languageserver": {
    "hoon-language-server": {
      "command": "hoon-language-server",
      "args": ["-p", "8080"],
      "filetypes": ["hoon"]
    }
  }
}
```

