# My life with a L-modle computer
```screenfetch
         _,met$$$$$gg.           passky@passky-pc
      ,g$$$$$$$$$$$$$$$P.        OS: Debian unstable sid
    ,g$$P""       """Y$$.".      Kernel: x86_64 Linux 5.9.0-1-amd64
   ,$$P'              `$$$.      Uptime: 5h 53m
  ',$$P       ,ggs.     `$$b:    Packages: 2996
  `d$$'     ,$P"'   .    $$$     Shell: zsh 5.8
   $$P      d$'     ,    $$P     Resolution: 1920x1848
   $$:      $$.   -    ,d$$'     DE: LXQt 0.14.1
   $$\;      Y$b._   _,d$P'      WM: Mutter
   Y$$.    `.`"Y$$$$P"'          WM Theme: Adwaita
   `$$b      "-.__               GTK Theme: Adwaita [GTK3]
    `Y$$                         Disk: 21G / 217G (11%)
     `Y$$.                       CPU: Intel Core i3-3217U @ 4x 1.8GHz [77.0°C]
       `$$b.                     GPU: Intel Corporation 3rd Gen Core processor Graphics Controller (rev 09)
         `Y$$b.                  RAM: 1535MiB / 1809MiB 
            `"Y$b._              SWAP: 1233MiB / 10000MiB
                `""""           
                                
```
我现在正用着一台2g内存的笔记本写着这篇文章,
令人高兴的是使用起来其实没有很恶心,虽然慢,但是工作的非常稳定.

以下是我的需求,和满足程度:
* 看480p视频和低清直播
> 满足程度: 一般,同时只能写markown和cc序列了;但冲浪体验还不错.
* 写一些比toy强一些的c/c++项目,中等复杂的数据结构.
> 满足程度: 极好.clangd出乎意料的非常节能,除了编译,写起来算是非常舒适.
* 写一些toy等级的java少文件练习
> 满足程度: 一般,虽然有jdtls,但这玩意儿吃内存而且体验确实不佳,导致写java的时候不能顺便放视频
* 写笔记并预览
> 满足程度: 满分

### 总结

linux比起吃内存不如说是吃cpu,只要cpu足够强大,swap吞吐的效率也会不错.
比如看视频的时候卡,从top观察,多半是cpu占用率过高,而不是其他原因.

使用软件:
* firefox
* emacs(build by myself,git-version)
* vim(build by myself,git-version)
* universal-ctags.gnu-global
* clangd,jdtls,rust-analyzer
