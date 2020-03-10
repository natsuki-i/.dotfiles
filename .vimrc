if &compatible
  set nocompatible
endif

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
set wildignore=*.o,*.lo,*.a,*.so
set wildmode=list:longest
set wildignorecase
highlight Pmenu ctermbg=magenta
highlight PmenuSel ctermbg=blue

" バッファ
set autoread
set hidden

" key mappings
let mapleader=","
noremap ; :
vnoremap ; :

" dein.vim

let s:dein_dir = expand('~/.cache/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
  execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  let g:rc_dir = expand('~/.dotfiles/.vim')
  let s:toml = g:rc_dir . '/dein.toml'
  let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'

  call dein#load_toml(s:toml, {'lazy': 0})
  call dein#load_toml(s:lazy_toml, {'lazy': 1})

  call dein#end()
  call dein#save_state()
endif

if dein#check_install()
  call dein#install()
endif

" YankRing
let g:yankring_manual_clipboard_check = 1
let g:yankring_history_dir = "~/.cache"

" Tsuquyomi
let g:tsuquyomi_completion_detail = 1

filetype plugin on
filetype indent on

autocmd BufNewFile,BufRead Gemfile setf ruby
autocmd BufRead,BufNewFile *.slim set filetype=slim
autocmd BufRead,BufNewFile *.coffee set filetype=coffee
autocmd FileType ruby,haml,slim,coffee setlocal sw=2 sts=2 ts=2 et makeprg=noglob\ rake
autocmd FileType typescript,markdown setlocal sw=4 sts=4 ts=4 et

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

