
Install
-------

```sh
yum install xsel # centos
brew install reattach-to-user-namespace # osx

git clone --recursive https://github.com/natsuki-i/.dotfiles.git ~/.dotfiles
```

### zshrc

```sh
ln -s ~/.dotfiles/.zshrc ~/.zshrc
```

### vimrc

```sh
ln -s ~/.dotfiles/.vimrc ~/.vimrc
ln -s ~/.dotfiles/.vim ~/.vim
vim -c "NeoBundleInstall" -c "q"
cd ~/.vim/.bundle/vimproc
make -f make_unix.mak
```

### tmux

```sh
ln -s ~/.dotfiles/.tmux.conf ~/.tmux.conf
```
