#!/bin/bash
function updatetogit()
{
    read -p "1 for gitee,2 for github,nothing for both:" x
	# if [ $# > 1 ]; then
	# 	echo "Wrong Input!"
	# 	updategit
	# fi

    # git remote add gitlab https://gitlab.com/passkyer/dotfiles.git
    git remote add gitee https://gitee.com/passkyw/config.git
    git remote add github https://github.com/Passky/Dotfiles.git
    git add -A
    git commit -am "Update"
    #git branch -D master
    #git branch -m master

	# default push force is BAD IDEA!!!
    if [[ ${x} == "1" ]]; then
        git pull gitee master
        git push gitee master
    elif [[ ${x} == "2" ]]; then
        git pull github master
        git push gitee master
        git push github master
    elif [[ ${x} == "3" ]]; then
        git push gitlab master
    else
        git pull gitee master
        git push gitee master
        git pull github master
        git push github master
        # git push gitlab master -f
    fi

    # mv ${PWD}/.vim/plugged/LeaderF/.gita  ${PWD}/.vim/plugged/LeaderF/.git
    # mv ${PWD}/.vim/plugged/coc.nvim/.gita  ${PWD}/.vim/plugged/coc.nvim/.git
}
updatetogit
