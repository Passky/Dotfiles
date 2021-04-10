#!/bin/bash
# copy from vimplus and heavily rewrite
# 获得发行版
function get_linux_distro()
{
	if grep -Eq "Deepin" /etc/*-release; then
		echo "UpstreamDebian"
	elif grep -Eq "uos" /etc/*-release; then
		echo "UpstreamDebian"
	elif grep -Eq "LinuxMint" /etc/*-release; then
		echo "UpstreamDebian"
	elif grep -Eq "elementary" /etc/*-release; then
		echo "UpstreamDebian"
	elif grep -Eq "Debian" /etc/*-release; then
		echo "UpstreamDebian"
	elif grep -Eq "Ubuntu" /etc/*-release; then
		echo "UpstreamDebian"
	elif grep -Eq "Kali" /etc/*-release; then
		echo "UpstreamDebian"
	elif grep -Eq "CentOS" /etc/*-release; then
		echo "CentOS"
	elif grep -Eq "fedora" /etc/*-release; then
		echo "fedora"
	elif grep -Eq "openSUSE" /etc/*-release; then
		echo "openSUSE"
	elif grep -Eq "Arch Linux" /etc/*-release; then
		echo "UpstreamArch"
	elif grep -Eq "arch" /etc/*-release; then
		echo "UpstreamArch"
	elif grep -Eq "debian" /etc/*-release; then # it is down case debian!
		echo "UpstreamArch"
	elif grep -Eq "ManjaroLinux" /etc/*-release; then
		echo "UpstreamArch"
	else
		echo "Unknow"
	fi

}

# 在linux平台上安装vim
function install_prepare_software_linux()
{
	distro=`get_linux_distro`
	echo "Linux distro: "${distro}
	if [ ${distro} == "UpstreamDebian"  ]; then
		install_prepare_software_on_debian
	elif [ ${distro} == "CentOS"  ]; then
		install_prepare_software_on_UpstreamRedhat
	elif [ ${distro} == "fedora"  ]; then
		install_prepare_software_on_UpstreamRedhat
	elif [ ${distro} == "openSUSE"  ]; then
		install_prepare_software_on_opensuse
	elif [ ${distro} == "UpstreamArch"  ]; then
		install_prepare_software_on_archlinux
	else
		echo "Not support linux distro: "${distro}
	fi

}

# 拷贝配置文件
function __Install_copy_file(){
	cp -nf ${PWD}/.* ~/
	rm ~/install.sh
	rm ~/gitupdate.sh

	rm -rf ~/.pip
	cp -rf ${PWD}/.pip ~/

	rm -rf ~/.ssh
	cp -rf ${PWD}/.ssh ~/

	rm -rf ~/.shellrc
	ln -s ${PWD}/.shellrc ~/

	rm -rf ~/.vimrc
	ln -s ${PWD}/.vimrc ~/

	rm -rf ~/.gitignore
	ln -s ${PWD}/.gitignore ~/

	rm -rf ~/.gitconfig
	ln -s ${PWD}/.gitconfig ~/

	rm -rf ~/.tmux.conf
	ln -s ${PWD}/.tmux.conf ~/

	rm -rf ~/.bashrc
	ln -s ${PWD}/.bashrc ~/

	rm -rf ~/.zsh
	ln -s ${PWD}/.zsh ~/

	rm -rf ~/.zshenv
	ln -s ${PWD}/.zshenv ~/

	rm ~/.zshrc -rf
	ln -s ${PWD}/.zshrc ~/

	rm -rf ~/.vim
	ln -s ${PWD}/.vim ~/

	rm -rf ~/.emacs.d
	ln -s ${PWD}/.emacs.d ~/

	mkdir ~/.config
    rm -r ~/.config/fish
	ln -s ${PWD}/.config/fish ~/.config/

    rm -r ~/.config/termite
	ln -s ${PWD}/.config/termite ~/.config/
	# mv ${PWD}/.vim/plugged/LeaderF/.gita  ${PWD}/.vim/plugged/LeaderF/.git
	# mv ${PWD}/.vim/plugged/coc.nvim/.gita  ${PWD}/.vim/plugged/coc.nvim/.git

	rm -rf ~/.config/nvim
	mkdir ~/.config/nvim
	#ln -s ${PWD}/.vim ~/.config/nvim
	ln -s ${PWD}/.vimrc ~/.config/nvim/init.vim
	ln -s ${PWD}/.vim/colors ~/.config/nvim/
	ln -s ${PWD}/.vim/plugged ~/.config/nvim/
	ln -s ${PWD}/.vim/autoload ~/.config/nvim/
	ln -s ${PWD}/.vim/coc-settings.json ~/.config/nvim/
	ln -s ${PWD}/.vim/setup ~/.config/nvim/
}

# 判断文件是否存在
function is_exist_file()
{
	filename=$1
	#-d $dirname
	if [ -f $filename  ]; then
		echo 1
	else
		echo 0
	fi

}

# 获取centos版本
function get_centos()
{
	version=`cat /etc/redhat-release | awk '{print $4}' | awk -F . '{printf "%s",$1}'`
	echo $version
}

# 判断是否是macos10.14版本
function is_macos1014()
{
	product_version=$(sw_vers | grep ProductVersion)
	if [[ $product_version =~ "10.14"  ]]; then
		echo 1
	else
		echo 0
	fi

}
# 安装mac平台必备软件
function install_prepare_software_on_mac()
{
	xcode-select --install
	brew install vim gcc cmake ctags ack git clang ccls
	macos1014=$(is_macos1014)
	if [ $macos1014 == 1  ]; then
		open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg
	fi
}

# 安装android平台必备软件
function install_prepare_software_on_android()
{
	pkg update
	pkg install -y  vim-python cmake python ctags ack-grep ncurses-utils zsh tmux git nodejs npm
}

# 安装debian必备软件
function install_prepare_software_on_debian()
{
	sudo apt-get update
	sudo apt-get upgrade -y
	sudo apt-get install -y cmake universal-ctags build-essential python3-dev fontconfig gdb python3-pip zsh clang tmux libx11-dev clang git wget curl
	sudo apt -y install nodejs npm neovim texinfo gnutls-bin zlibc
	sudo apt -y install clangd ccls
}

# 安装必备软件
function install_prepare_software_on_UpstreamRedhat()
{
    #	version=$(get_UpstreamRedhat_version)
    # sudo dnf install -y epel-release
		sudo dnf install -y ctags automake gcc gcc-c++ kernel-devel cmake3 python3-devel fontconfig ack zsh tmux clang perl git ncurses-devel perl-devel perl-ExtUtils-Embed libXt-devel gnutls-devel texinfo
		sudo dnf install -y neovim perl npm texinfo
}

# 安装archlinux必备软件
function install_prepare_software_on_archlinux()
{
	sudo pacman -S --needed --noconfirm vim ctags automake gcc cmake python ack fontconfig cmake gdb llvm clang zsh tmux yarn npm perl python-pip
	sudo pacman -S --needed --noconfirm ccls neovim
	#sudo ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5
}

# 安装opensuse必备软件
function install_prepare_software_on_opensuse()
{
	sudo zypper install -y vim ctags gcc gcc-c++ cmake python-devel python3-devel ack fontconfig  ncurses5-devel gdb  zsh tmux clang npm
	sudo zypper install -y  nodejs yarn neovim
}

# 开始安装myvim
function begin_install_myvim()
{
	source ~/.shellrc
	read -p "type 1 to install_prepare_software_linux" x
	if [[ ${x} == '1' ]]; then
		install_prepare_software_linux
		pip3 install jedi
		pip3 install neovim
		pip3 install ptvsd pytest
		pip3 install pygments yapf pylint
	fi
	read -p "type 1 to compile newest vim" x
	if [[ ${x} == '1' ]]; then
		update_vim
	fi
	read -p "type 1 to install coc-plugins" x
	if [[ ${x} == '1' ]]; then
		vim -c "CocInstall coc-git coc-python coc-pairs coc-json coc-cmake coc-lists coc-highlight coc-tsserver coc-vimlsp coc-sh coc-snippets coc-yaml coc-syntax"
	fi
	clear
	echo 'Finished!'
	echo 'Basic Vim & Basic System Setup Done!'
}

# 获取当前时间戳
function get_now_timestamp()
{
	cur_sec_and_ns=`date '+%s-%N'`
	echo ${cur_sec_and_ns%-*}

}

# main function
function main()
{
	begin=`get_now_timestamp`
	type=$(uname)
	echo "Platform type: "${type}
	read -p "type 1 to cp" x
	if [[ ${x} == '1' ]]; then
		__Install_copy_file
	fi
	#git clone --depth=1 https://gitee.com/romkatv/powerlevel10k.git ~/.__powerlevel10k
	if [ ${type} == "Darwin"  ]; then
		install_prepare_software_mac
		begin_install_myvim
	elif [ ${type} == "Linux"  ]; then
		tp=$(uname -a)
		if [[ $tp =~ "Android"  ]]; then
			echo "Android"
			install_prepare_software_android
			begin_install_myvim
		else
			begin_install_myvim
		fi
	fi
	end=`get_now_timestamp`
	second=`expr ${end} - ${begin}`
	min=`expr ${second} / 60`
	echo "It takes "${min}" minutes."
}

# 调用main函数
main
