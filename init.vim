call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-sensible'

Plug 'iCyMind/NeoSolarized'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'roxma/nvim-completion-manager'
Plug 'roxma/ncm-clang'
Plug 'Shougo/neco-vim'

Plug 'arakashic/chromatica.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'w0rp/ale'

Plug 'jiangmiao/auto-pairs'

Plug 'tpope/vim-vinegar'

Plug 'tpope/vim-surround'

Plug 'tpope/vim-commentary'

Plug 'terryma/vim-multiple-cursors'

Plug 'yggdroot/indentline'

call plug#end()

set nocompatible

set termguicolors
set background=dark
colorscheme NeoSolarized

set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20

let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled=1

let g:chromatica#libclang_path = '/usr/lib/llvm-4.0/lib/libclang.so'
let g:chromatica#enable_at_startup=1
let g:chromatica#highlight_feature_level = 1

let g:ale_sign_column_always = 1

set number

let g:multi_cursor_quit_key='<C-h>'

let g:indentLine_setConceal = 1
let g:indentLine_char = '▏'

set tabstop=4
set shiftwidth=4
set expandtab

set showmatch
