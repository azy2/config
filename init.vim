call plug#begin('~/.local/share/nvim/plugged')

Plug 'tpope/vim-sensible'

Plug 'iCyMind/NeoSolarized'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'roxma/nvim-completion-manager'
Plug 'roxma/ncm-clang'
Plug 'Shougo/neco-vim'
Plug 'racer-rust/vim-racer'
Plug 'roxma/nvim-cm-racer'

Plug 'w0rp/ale'

Plug 'jiangmiao/auto-pairs'

Plug 'tpope/vim-vinegar'

Plug 'tpope/vim-surround'

Plug 'tpope/vim-commentary'

Plug 'terryma/vim-multiple-cursors'

Plug 'yggdroot/indentline'

Plug 'rust-lang/rust.vim'

call plug#end()

set nocompatible

set termguicolors
set background=dark
colorscheme NeoSolarized

set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20

let g:airline_theme='solarized'
let g:airline#extensions#tabline#enabled=1

let g:ale_sign_column_always = 1
let g:ale_linters = {'rust':['rls', 'rustfmt', 'cargo', 'rustc']}

set number

let g:multi_cursor_quit_key='<C-h>'

let g:indentLine_setConceal = 1
let g:indentLine_char = '▏'

set tabstop=4
set shiftwidth=4
set expandtab

set showmatch
