syntax on
set number
set tabstop=4

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
call vundle#end()            " required

filetype plugin indent on    " required

set background=dark


let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line




" Standalone vim snippet for php and html files.
" Add this to your home directory's .vimrc
"
" May conflict with other syntax checking plugins.
" Need to use absolute path to phan_client, or put it in your path (E.g. $HOME/bin/phan_client)
" This is based off of a snippet mentioned on http://vim.wikia.com/wiki/Runtime_syntax_check_for_php

" Note: in Neovim, instead use %m\ in\ %f\ on\ line\ %l
au FileType php,html setlocal makeprg=phan_client
au FileType php,html setlocal errorformat=%m\ in\ %f\ on\ line\ %l,%-GErrors\ parsing\ %f,%-G

au! BufWritePost  *.php,*.html    call PHPsynCHK()

function! PHPsynCHK()
  let winnum =winnr() " get current window number
  " or 'silent make --disable-usage-on-error -l %' in Phan 0.12.3+
  silent make -l %
  cw " open the error window if it contains an error. Don't limit the number of lines.
  " return to the window with cursor set on the line of the first error (if any)
  execute winnum . "wincmd w"
  :redraw!
endfunction
