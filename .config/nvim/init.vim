syntax on
set number
set tabstop=2
set expandtab
set shiftwidth=2
set nocompatible              " be iMproved, required
filetype off                  " required
filetype plugin indent on     " required
set background=dark


" set the runtime path to include Vundle and initialize
" set rtp+=~/.vim/bundle/Vundle.vim

" call vundle#begin()
" Plugin 'VundleVim/Vundle.vim'
" Plugin 'flazz/vim-colorschemes'
" Plugin 'dense-analysis/ale'
" Plugin 'junegunn/fzf'
" Plugin 'junegunn/fzf.vim'
" Plugin 'hashivim/vim-terraform'
" Plugin 'rust-lang/rust.vim'
" call vundle#end()            " required


if empty(glob('~/.vim/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'VundleVim/Vundle.vim'
Plug 'flazz/vim-colorschemes'
Plug 'dense-analysis/ale'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'hashivim/vim-terraform'
Plug 'rust-lang/rust.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()


" === Plugin settings ===

" vim-terraform
let g:terraform_fmt_on_save=1

" rust
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

