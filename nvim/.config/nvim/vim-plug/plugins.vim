call plug#begin('~/.config/nvim/plugged') 
 " Plugin section
 Plug 'scrooloose/nerdtree' " Nav bar
 Plug 'ryanoasis/vim-devicons' " Filetype icons on nav bar
 Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
 Plug 'junegunn/fzf.vim' " Fuzzy finder pt2
" Plug 'townk/vim-autoclose'
 Plug 'sheerun/vim-polyglot'
 Plug 'Guzzii/python-syntax'
 Plug 'vim-airline/vim-airline' " Enable airline
 Plug 'vim-airline/vim-airline-themes' " Add airline themes
" Plug 'kaicataldo/material.vim' " Material them
call plug#end()

let g:python_highlight_all = 1

" Fix for material color scheme
if (has('nvim'))
    let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

if (has('termguicolors'))
    set termguicolors
endif


" Material theme config
"let g:material_terminal_italics = 1
"let g:material_theme_style = 'palenight'

" air-line
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" Set color scheme
colorscheme material
let g:airline_theme='material'

" Dont alter background color
" hi Normal guibg=NONE
" hi LineNr guifg=#676e95
" hi CursorLineNr guifg=#89ddff

" Nerd tree config
let g:NERDTreeShowHidden = 1 " Show hidden files
let g:NERDTreeMinimalUI = 1 " Enable minimal ui
let g:NERDTreeIgnore = []
let g:NERDTreeStatusLine = ''
" Automatically close nvim if only NERDTree is open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
nnoremap <silent> <C-b> :NERDTreeToggle<CR> " Toggle nerdtree

" Fuzzy finder config
nnoremap <C-p> :FZF<CR>
let g:fzf_action = {
 \ 'ctrl-s': 'split',
 \ 'ctrl-v': 'vsplit'
 \}
let $FZF_DEFAULT_COMMAND = 'ag -g ""' " Use silversearcher (ignores files in .gitignore)

" Setup coc
let g:coc_global_extensions = ['coc-css', 'coc-json', 'coc-python', 'coc-java', 'coc-prettier']
