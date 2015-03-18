
" タブ
set tabstop=2
set shiftwidth=2
set smarttab
set autoindent
set smartindent
set expandtab

" 文字コード
set fileformat=unix
set fileformats=unix,dos,mac
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8,sjis,euc-jp,iso2022-jp

" 検索
set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
set infercase
nmap <Esc><Esc> ;nohlsearch<CR><Esc>

" 表示
set number
set cursorline
set cmdheight=2
set laststatus=2
set statusline=%<%f\ %m%r%h%w
set statusline+=%{'['.(&fenc!=''?&fenc:&enc).']['.&fileformat.']'}
set statusline+=%=%l/%L,%c%V%8P
"set showtabline=2
set wildmenu
highlight Pmenu ctermbg=magenta
highlight PmenuSel ctermbg=blue

" バッファ
set autoread
set hidden

" key mappings
let mapleader=","
noremap ; :
vnoremap ; :

" NeoBundle
set nocompatible
filetype off

if has('vim_starting')
  set runtimepath+=~/.dotfiles/.vim/bundle/neobundle.vim
endif

call neobundle#begin(expand('~/.vim/.bundle'))

NeoBundle 'Shougo/unite.vim.git'
NeoBundle 'Shougo/neocomplcache.git'
NeoBundle 'Shougo/neocomplcache-clang'
NeoBundle 'Shougo/git-vim.git'
NeoBundle 'Shougo/vimproc.git'
NeoBundle 'Shougo/vimshell.git'
NeoBundle 'Shougo/vinarise.git'
NeoBundle 'vim-scripts/YankRing.vim.git'
NeoBundle 'vim-scripts/javacomplete.git'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle "mattn/emmet-vim"
NeoBundle "slim-template/vim-slim"
NeoBundle "kchmck/vim-coffee-script"
NeoBundle "nathanaelkane/vim-indent-guides"
NeoBundle 'tomasr/molokai'

call neobundle#end()

filetype plugin on
filetype indent on

" neocomplcache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
let g:neocomplcache_dictionary_filetype_lists = {
	\ 'default' : '',
	\ 'vimshell' : $HOME.'/.vimshell_hist',
	\ }
if !exists('g:neocomplcache_keyword_patterns')
	let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'
imap <C-k> <Plug>(neocomplcache_snippets_expand)
smap <C-k> <Plug>(neocomplcache_snippets_expand)
inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()
inoremap <expr><CR> neocomplcache#smart_close_popup() . "\<CR>"
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplcache#close_popup()
inoremap <expr><C-e> neocomplcache#cancel_popup()
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
if !exists('g:neocomplcache_omni_patterns')
	let g:neocomplcache_omni_patterns = {}
endif

let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '\%(\.\|->\)\h\w*'
let g:neocomplcache_omni_patterns.cpp = '\h\w*\%(\.\|->\)\h\w*\|\h\w*::'

" YankRing
let g:yankring_manual_clipboard_check = 1
let g:yankring_history_dir = "~/.vim/"

" javacomplete
autocmd FileType java setlocal omnifunc=javacomplete#Complete
autocmd FileType java setlocal completefunc=javacomplete#CompleteParamsInfo

autocmd BufNewFile,BufRead Gemfile setf ruby
autocmd BufRead,BufNewFile *.slim set filetype=slim
autocmd BufRead,BufNewFile *.coffee set filetype=coffee
autocmd FileType ruby,haml,slim,coffee setlocal sw=2 sts=2 ts=2 et makeprg=noglob\ rake

autocmd FileType c,cpp setlocal sw=4 sts=4 ts=4 noet

" 色指定
set background=dark
colorscheme molokai
syntax on
hi Normal ctermbg=none
hi Visual ctermbg=252 ctermfg=232
hi Pmenu ctermbg=238

" カーソル行の表示がおかしい時は以下をアンコメント
"set nocursorline

" インデントの深さに色を付ける
let g:indent_guides_start_level=2
let g:indent_guides_auto_colors=0
let g:indent_guides_enable_on_vim_startup=0
let g:indent_guides_color_hex_guibg_pattern=2
let g:indent_guides_guide_size=1
let g:indent_guides_space_guides=1
hi IndentGuidesOdd  ctermbg=239
hi IndentGuidesEven ctermbg=008
nmap <silent><Leader>ig <Plug>IndentGuidesToggle
autocmd FileType coffee,ruby,javascript,slim,haml IndentGuidesEnable

" 80,100,120桁目に線を引く
set colorcolumn=80,100,120

" 行末のスペースを強調表示する
set list
set listchars=tab:^\ ,trail:~

" ruby向けの折りたたみ
function! s:FoldRubyBlock()
  let start = search('^\s*\(\(describe\|context\|it\|shared_examples_for\|shared_context\|should\|specify\|scenario\|before\|after\).*do\)\|\(\(class\|module\|def\) .*$\)$', 'We')
  let nest = match(getline('.'), 'class\|module\|describe\|context\|shared_examples_for\|shared_context')
  let indent = indent(start)
  let end   = search('^\s\{'.indent.'\}end$', 'We')
  let cmd   = (start).','.(end).'fold'
  if (start > 0) && (start < end)
    execute cmd
    if nest != -1
      execute "normal zo"
      call cursor(start+1,1)
    endif
    return 1
  else
    return 0
  endif
endfunction

function! FoldAllRubyBlocks()
  let position = line('.')
  exe cursor(1,1)
  let result = 1

  while result == 1
    let result = s:FoldRubyBlock()
  endwhile

  call cursor(position, 1)
endfunction

function! s:FoldAllRubyBlocksOnLoad()
  execute "normal zE"
  call FoldAllRubyBlocks()
endfunction

autocmd FileType ruby call s:FoldAllRubyBlocksOnLoad()

" python向けの折りたたみ
autocmd FileType python set fdm=indent fdl=99 cinw=if,elif,else,for,while,try,except,finally,def,class

" 畳み込みの文字列を変更する
hi Folded term=standout ctermfg=4 ctermbg=0
set fillchars=vert:\|
set foldtext=getline(v:foldstart)

" local settings
if filereadable($HOME.'/.vimrc_local.vim')
	source $HOME/.vimrc_local.vim
endif

