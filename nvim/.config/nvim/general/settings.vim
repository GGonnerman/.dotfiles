" Disable backwards compatibility with vi
set nocompatible

" Enable filetype plugin and indent
filetype plugin on
filetype plugin indent on

" Auto read when a file is changed from external programs
set autoread
au FocusGained,BufEnter * checktime

" Enable syntax highlighting
syntax enable

set foldmethod=marker
nnoremap <space> za
vnoremap <space> zf

" Don't copy stuff when deleting
nnoremap d "_d
vnoremap d "_d

" :W sudo saving for permission-denied errors
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!

" Set 4 line padding around cursor when moving vertically (j/k)
set so=4

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,*/.git/*,*/.hg/*,*/.svn/*,*.DS_Store

" Always show curent position
set ruler

" height of command bar
set cmdheight=1

" Bugger becomes hiddenw ehn it is abandoned
set hid

" Don't redraw while executing macros (for performance)
set lazyredraw

" Rebind alt+hjkl to move across vim splits
tnoremap <A-h> <C-\><C-n><C-w>h
tnoremap <A-j> <C-\><C-n><C-w>j
tnoremap <A-k> <C-\><C-n><C-w>k
tnoremap <A-l> <C-\><C-n><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

" Set shared clipboard
set clipboard=unnamedplus

" Enable hybrid line numbers
set number relativenumber
set nu rnu

" Enable mouse scrolling
set mouse=a

" Enable folding
" set foldmethod=marker
" nnoremap <space> za

" No annoying sounds on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Set utf8 as the standard encoding
set encoding=utf8

" Use Unix as the standard filetype
set ffs=unix,dos,mac

" Turn off backups
set nobackup
set nowb
set noswapfile

" Wrap lines
set wrap

" Highlight matching brace
set showmatch " Show matching brackets when over them
set mat=2 " Blink for 2/10th of a second on matching brackets
set hlsearch " Highlight search results
set ignorecase " Always case-insensitive
set smartcase " Enable smart-case search
set incsearch " Searches for strings incrementally

" Indentation
set autoindent " Auto indent new lines
set shiftwidth=4 " Number of auto-indent spaces
set smartindent " Enable smart indents
set smarttab " Enable smart tabs
set softtabstop=4 " # of spaces per tab

set undolevels=1000 " Number of undo levels
set backspace=eol,start,indent " Backspace behavior
set whichwrap+=<,>,h,l

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

set noswapfile " Remove swap files

noremap 0 ^
noremap ^ 0

" Remove trailing whitespace on save
fun! CleanExtraSpaces()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    silent! %/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfun

if has("autocmd")
    autocmd BufWritePre *.txt,*.js,*.py,*.sh :call CleanExtraSpaces()
endif

" TODO: Make this cleaner
autocmd FileType python nnoremap <silent> <F9> :call SaveAndExecute("python")<CR>
autocmd FileType javascript nnoremap <silent> <F9> :call SaveAndExecute("javascript")<CR>
autocmd FileType java nnoremap <silent> <F9> :call SaveAndExecute("java")<CR>
autocmd FileType python inoremap <silent> <F9> <esc>:call SaveAndExecute("python")<CR>
autocmd FileType javascript inoremap <silent> <F9> <esc>:call SaveAndExecute("javascript")<CR>
autocmd FileType java inoremap <silent> <F9> <esc>:call SaveAndExecute("java")<CR>

autocmd bufenter * if (winnr("$") == 1 && exists("b:output") && b:output == "primary") | q | endif


function! SaveAndExecute(language)
    " SOURCE [reusable window]: https://github.com/fatih/vim-go/blob/master/autoload/go/ui.vim

    " save and reload current file
    silent execute "update | edit"

    " get file path of current file
    let s:current_buffer_file_path = expand("%")

    let s:output_buffer_name = a:language
    let s:output_buffer_filetype = "output"

    " reuse existing buffer window if it exists otherwise create a new one
    if !exists("s:buf_nr") || !bufexists(s:buf_nr)
        silent execute 'botright new ' . s:output_buffer_name
        let s:buf_nr = bufnr('%')
    elseif bufwinnr(s:buf_nr) == -1
        silent execute 'botright new'
        silent execute s:buf_nr . 'buffer'
    elseif bufwinnr(s:buf_nr) != bufwinnr('%')
        silent execute bufwinnr(s:buf_nr) . 'wincmd w'
    endif

    silent execute "setlocal filetype=" . s:output_buffer_filetype
    setlocal bufhidden=delete
    setlocal buftype=nofile
    setlocal noswapfile
    setlocal nobuflisted
    setlocal winfixheight
    setlocal cursorline " make it easy to distinguish
"   setlocal nonumber
"   setlocal norelativenumber
    setlocal showbreak=""

    " clear the buffer
    setlocal noreadonly
    setlocal modifiable
    %delete _

    " add the console output
    if a:language =~ "python"
        silent execute ".!python " . shellescape(s:current_buffer_file_path, 1)
    elseif a:language =~ "javascript"
        silent execute ".!node " . shellescape(s:current_buffer_file_path, 1)
    elseif a:language =~ "java"
        silent execute ".!java " . shellescape(s:current_buffer_file_path, 1)
    endif

    let b:output = "primary"

    silent execute "resize 5"
    silent execute "wincmd k"



    " resize window to content length
    " Note: This is annoying because if you print a lot of lines then your code buffer is forced to a height of one line every time you run this function.
    "       However without this line the buffer starts off as a default size and if you resize the buffer then it keeps that custom size after repeated runs of this function.
    "       But if you close the output buffer then it returns to using the default size when its recreated
    "execute 'resize' . line('$')

    " make the buffer non modifiable
"   setlocal readonly
"   setlocal nomodifiable
endfunction

" Language autorun support
augroup rungroup
    autocmd!
    autocmd BufRead,BufNewFile *.js nnoremap <F5> :exec '!node' shellescape(@%, 1)<cr>
    autocmd BufRead,BufNewFile *.py nnoremap <F5> :exec '!python' shellescape(@%, 1)<cr>
augroup END
