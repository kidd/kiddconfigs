so ~/.vim/plugin/supertab.vim
" An example for a vimrc file.
"
" Maintainer:   Bram Moolenaar <Bram@vim.org>
" Last change:  2006 Nov 16
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"         for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"       for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
set path+=../../../root/src/,../../../root/ "catalyst"
set path+=/usr/include/eo/
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("vms")
  set nobackup      " do not keep a backup file, use versions instead
  "else
  "set backup       " keep a backup file
endif
set history=50      " keep 50 lines of command line history
set ruler       " show the cursor position all the time
set showcmd     " display incomplete commands
set incsearch       " do incremental searching

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" In many terminal emulators the mouse works just fine, thus enable it.
set mouse=a

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
"if &t_Co > 2 || has("gui_running")
syntax on
set hlsearch
"endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent        " always set autoindenting on

endif " has("autocmd")
  set nobackup      " do not keep a backup file, use versions instead

inoremap jk <esc>
nmap ,vimrc :new ~/.vimrc<cr>
let mapleader = ","
map <F11> :set paste!<cr>:set paste?<cr>
imap <F11> <esc>:set paste!<cr>:set paste?<cr>i
nnoremap ,ñ :h perlsupport-mappings<cr>
nmap Y y$
imap <F3> <c-o>:nohl<cr>
nmap <F3> :nohl<cr>
nnoremap ,mode mmGo# vim: set tabstop=4 shiftwidth=4 foldmethod=marker : ##<esc>`m
set guiheadroom=0
nmap <F1> :bp<cr>
nmap <F2> :bn<cr>
nmap ,tn :tabnew<cr>
iab RGC Raimon Grau Cuscó
iab raimail raimonster@gmail.com
iab iraimail rgrau@intelligentpharma.com
nnoremap ,pac ma:set tw=70<cr>gqgqV'a:s/^/$NAME: /<cr>  
set ts=4
set sw=4
set tw=100
vmap ,code I[code]<cr><esc>gv<esc>o[/code]
"Edit another file in the same directory as the current file
"   uses expression to extract path from current file's path
"  (thanks Douglas Potts)
if has("unix")
    map ,e :e <C-R>=expand("%:p:h") . "/" <CR>
else
    map ,e :e <C-R>=expand("%:p:h") . "\" <CR>
endif 

set go-=m
set go-=T
nnoremap =p ='[
let g:Tex_DefaultTargetFormat='pdf' 
nmap ,ls :ls<cr>b
set iskeyword+=:
set grepprg=ack\ -a
set autoread
set hidden
set scrolloff=2                 " context

set showcmd " Show current uncompleted command? Absolutely!
set showmatch " Show the matching bracket for the last ')'?
set showmode " Show the current mode? YEEEEEEEEESSSSSSSSSSS!
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.cmi,.cmo " Suffixes to ignore in file completion

if has("autocmd") && exists("+omnifunc")
    autocmd Filetype *
                \   if &omnifunc == "" |
                \       setlocal omnifunc=syntaxcomplete#Complete |
                \   endif
endif

set encoding=utf8

nnoremap ,mr :FuzzyFinderMruFile<cr>
nnoremap ,o :FuzzyFinderFile<cr>
nnoremap ,b :FuzzyFinderBuffer<cr>
"colorscheme guardian
colorscheme inkpot
set foldmethod=marker

hi Pmenu        guifg=grey80 guibg=grey20
hi PmenuSel     guifg=#dcdccc guifg=grey10
hi PmenuSbar    guibg=#dcdccc
hi PmenuThumb   guifg=#dcdccc

set wildmenu
nmap ,tn :tabnew<cr>
set hls
map ,cd :cd %:p:h<cr>
set cursorline

if !filereadable("Makefile") && !filereadable("makefile") 
    setlocal makeprg=g++\ -o\ %<\ %\ -lm
    "setlocal makeprg=gcc\ -o\ %<\ %\ -lm
endif

function! Vim_execute()
    execute getline('.')
endfunction
noremap <LocalLeader>v :call Vim_execute()<CR>
let LocalLeader=","
let Leader=","
nnoremap ,t :Tlist<cr>

"emacsy insert mode binds
cnoremap <c-a> <c-b>
inoremap <c-a> <c-o>I
inoremap <c-e> <c-o>A
inoremap <a-b> <c-o>b
inoremap <a-f> <c-o>w
inoremap <a-d> <c-o>dw
au FileType help map <buffer> <cr> <c-]>
let NERDShutUp=1

if exists('+autochdir')
    set autochdir
else
    autocmd BufEnter * silent! lcd %:p:h:gs/ /\\ /
endif

map <F12> :w<cr>:!%<cr>
au BufRead,BufNewFile *.viki set ft=viki
inoremap ,tt [%%]<left><left>  <left>
inoremap <a-l> <right>
inoremap <a-h> <left>
set isfname-==
map ,ww :e ~/.vim/Viki/index<cr>

"""" cpp headers
"if !exists("autocommands_loaded")
  "let autocommands_loaded = 1
  "au BufNewFile *.h call InsertCHHeader()
  "au BufWrite *.h call ModifyTime()
  "" You might want to comment-out the line below - see note 6 at the end of the post.
  "au BufReadPost *.h call ModifyHeader()
"endif

"function! InsertCHHeader()
  "call InsertSkeleton("skeleton.h") " CHANGE this!
  "call InsertFname()
  "1
  "" Search for Description
  "call search("Description:")
  "normal $
  "startinsert
"endfunction

"function! InsertSkeleton(fname)
  "let path_to_skeletons = $HOME . "/etc/skeletons/" " CHANGE this!
  "" Save cpoptions
  "let cpoptions = &cpoptions
  "" Remove the 'a' option - prevents the name of the
  "" alternate file being overwritten with a :read command
  "exe "set cpoptions=" . substitute(cpoptions, "a", "", "g")
  "exe "read " . path_to_skeletons . a:fname
  "" Restore cpoptions
  "exe "set cpoptions=" . cpoptions
  "" Delete the first line into the black-hole register
  "1, 1 delete _
  "" Search for Filename:
  "call search("Filename:")
  "exe "normal A " . expand("%:t")
  "" Search for Created:
  "let current_time = strftime("%b %d %Y %T") "CHANGE this!
  "call search("Created:")
  "exe "normal A " . current_time
  "" Search for Last modified:
  "call search("Last modified:")
  "exe "normal A " . current_time

  "" Search for Date
  "let date_line_no = search("Date")
  "let rev_history = getline(line("."))
  "let rev_history = substitute(rev_history, "Date ", strftime("%b %d %Y"), "") " CHANGE this!
  "let rev_history = substitute(rev_history, "Author", "KG ", "") "CHANGE this!
  "let rev_history = substitute(rev_history, "Remarks", "File created.", "")
  "call append(date_line_no, rev_history)
"endfunction

"function! InsertFname()
  "" Convert newname.h to _NEWNAME_H_
  "let fname = expand("%:t")
  "let fname = toupper(fname)
  "let fname = substitute(fname, "\\.", "_", "g")
  "" Search for #ifndef
  "call search("#ifndef")
  "exe "normal A " . "_" . fname . "_"
  "" Search for #define
  "call search("#define")
  "exe "normal A " . "_" . fname . "_"
"endfunction

"function! ModifyHeader()
  "" Modify header only if we have write permissions
  "if &readonly == 0
    "" Search for Date
    "let date_line_no = search("Date")
    "if date_line_no != 0
      "let rev_history = getline(line("."))
      "" Substitute Date, and Author fields
      "let rev_history = substitute(rev_history, "Date ", strftime("%b %d %Y"), "") " CHANGE this!
      "let rev_history = substitute(rev_history, "Author", "KG ", "") " CHANGE this!
      "let rev_history = substitute(rev_history, "Remarks", "", "")
      "" echo "Modified = " . rev_history
      "call append(date_line_no, rev_history)
      "normal j$
      "startinsert
    "endif
  "endif
"endfunction

"function! ModifyTime()
  "" Do the updation only if the current buffer is modified
  "if &modified == 1
    "let current_time = strftime("%b %d %Y %X") " CHANGE this!
    "" Save current position at mark i
    "normal mi
    "" Search for Last modified:
    "let modified_line_no = search("Last modified:")
    "if modified_line_no != 0 && modified_line_no < 10
      "" There is a match in first 10 lines
      "" Go to the : in modified:
      "exe "normal f:2l" . strlen(current_time) . "s" . current_time
      "echo "Modified date stamp to " . current_time
      "sleep 500m
      "" Restore position
      "normal `i
    "endif
  "endif
"endfunction

nnoremap ,ñ :h csupport-usage-vim<cr>

set grepprg=ack\ -a
imap <c-l> <right>
set hlsearch
set smartcase
map <C-F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q --languages=c++ /usr/include /usr/local/include <CR>
nnoremap <c-s> :w<cr>:make<cr>
nnoremap ñ :w<cr>
nnoremap <c-ñ> :make<cr>
nnoremap Ñ :
nnoremap ,wm :w<cr>:make<cr>
map [[ ?{<CR>w99[{
map ][ /}<CR>b99]}
map ]] j0[[%/{<CR>
map [] k$][%?}<CR>
let g:DoxygenToolkit_blockHeader="--------------------------------------------------------------------------"
let g:DoxygenToolkit_blockFooter="----------------------------------------------------------------------------"
let g:DoxygenToolkit_authorName="Raimon Grau Cuscó"
"let g:DoxygenToolkit_licenseTag="\<enter>" 
let g:DoxygenToolkit_undocTag="DOXIGEN_SKIP_BLOCK"
"let g:DoxygenToolkit_commentType="C++"
let g:DoxygenToolkit_briefTag_funcName = "yes"  " to include function name between briefTag_pre and briefTag_post
nnoremap <c-l> zz
inoremap <c-l> <c-o>zz
let g:C_OutputGvim='buffer'
set nu
"set list
"set listchars=tab:\|\ 
set laststatus=2
"let Tlist_Auto_Update = 0
let Tlist_Exit_OnlyWindow = 1
"let Tlist_File_Fold_Auto_Close = 1
let Tlist_Show_One_File = 1

"---------------------------------
"Use one of the following to define the camel characters.
"Stop on capital letters.
:let g:camelchar = "A-Z"
"Also stop on numbers.
:let g:camelchar = "A-Z0-9"
"Include '.' for class member, ',' for separator, ';' end-statement,
"and <[< bracket starts and "'` quotes.
:let g:camelchar = "A-Z0-9.,;:{([`'\""

nnoremap <silent><C-Left> :<C-u>cal search('\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%^','bW')<CR>

nnoremap <silent><C-Right> :<C-u>cal search('\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%$','W')<CR>

inoremap <silent><C-Left> <C-o>:cal search('\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%^','bW')<CR>

inoremap <silent><C-Right> <C-o>:cal search('\<\<Bar>\%(^\<Bar>[^'.g:camelchar.']\@<=\)['.g:camelchar.']\<Bar>['.g:camelchar.']\ze\%([^'.g:camelchar.']\&\>\@!\)\<Bar>\%$','W')<CR>

set sm
set scs
iab {{ {<cr>}<up><cr>
nnoremap ,1 :set ft=html.tt
au BufNewFile,BufRead *.tt2 setf tt2
au BufNewFile,BufRead *.tt setf tt2
inoremap {{ {}<left><cr><cr><up>
iab nperl <esc><F11>I#!/usr/bin/perl<cr>use strict;<cr>use warnings;<esc><F11>o
nmap +e mz/^use Test::More tests<cr><c-a>'z
nnoremap ,n :NERDTreeToggle<cr>
let g:ctags_exe='ctags'

