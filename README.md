This document describes the various components of our LSP server implementation.

# hoon-language-server

Earth-side component of a language server for Hoon. Enables two-way communication between supported text editors and the `language-server` agent on an urbit ship.

## Installation

### npm

```
npm install -g @urbit/hoon-language-server
```

### nix

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

## Urbit Setup

You must have an urbit ship running (can be a livenet ship) with the `language-server` agent started.  To create and start a fake `~zod`:

```
urbit -F zod -c zod
```

If you want to use the bleeding edge version of the language server, install the `argo` desk from `~dister-dozzod-middev` - `|install ~dister-dozzod-middev %argo`". After installation the `%language-server` agent should be running.

*skip the following if you've installed the argo desk*

In the urbit dojo, start the language server:

```
dojo> |start %language-server
```

get the `+code`

```
dojo> +code
```

To start the same ship again in the future just run:

```
urbit zod
```

in the same directory it was created in.

## Editor Setup

Your code editor now needs to use `hoon-language-server` as an LSP provider. Supported plugins:
### VSCode

- [hoon-vscode](https://github.com/famousj/hoon-vscode)
- [hoon-assist-vscode](https://github.com/urbit/hoon-assist-vscode)

### Emacs

- [hoon-mode.el](https://github.com/urbit/hoon-mode.el)

### Vim

- [hoon.vim](https://github.com/urbit/hoon.vim)

`hoon.vim` does not use the language server itself, but the github page describes a setup using [vim-lsp](https://github.com/prabirshrestha/vim-lsp).

### Neovim

Neovim users should use [hoon.vim](https://github.com/urbit/hoon.vim) with one of the following LSP setups:

#### Native LSP

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

#### coc.nvim

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



# hoon-vscode

Hoon language support in VSCode.

## Requirements

```
  npm i -g @urbit/hoon-language-server
```
## Configuration

Configuration is done in VSCode's extension settings. The following needs to be provided in order to correctly launch the language server and connect to a running urbit ship:

- @p
- HTTP port (the extension assumes the ship is running locally)
- code (retrieved by running `+code` in your ship's dojo)

The extension will attempt to run `hoon-language-server` with the above arguments, so make sure it's installed and discoverable by VSCode. 

## Features

- Syntax highlighting for all your favorite runes.
- Automatic closing of all your `sel`s and `ser`s.
- LSP integration.

### 0.1.0

Initial release of Hoon
