" url.vim
" @Author:      Thomas Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2008-06-15.
" @Revision:    0.0.25

if &cp || exists("loaded_tlib_url_autoload")
    finish
endif
let loaded_tlib_url_autoload = 1


" TODO: These functions could use printf() now.

" Decode an encoded URL.
function! tlib#url#Decode(url) "{{{3
    return substitute(a:url, '\(+\|%\(%\|\x\x\)\)', '\=tlib#url#DecodeChar(submatch(1))', 'g')
endf


" Decode a single character.
function! tlib#url#DecodeChar(char) "{{{3
    if a:char == '%%'
        return '%'
    elseif a:char == '+'
        return ' '
    else
        return nr2char("0x".a:char[1 : -1])
    endif
endf


" Encode a single character.
function! tlib#url#EncodeChar(char) "{{{3
    if a:char == '%'
        return '%%'
    elseif a:char == ' '
        return '+'
    else
        return printf("%%%X", char2nr(a:char))
    endif
endf


" Encode an url.
function! tlib#url#Encode(url, ...) "{{{3
    TVarArg ['extrachars', '']
    let rx = '\([^a-zA-Z0-9_.'. extrachars .'-]\)'
    " TLogVAR a:url, rx
    let rv = substitute(a:url, rx, '\=tlib#url#EncodeChar(submatch(1))', 'g')
    " TLogVAR rv
    return rv
endf


