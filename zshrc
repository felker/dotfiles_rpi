# ~/.zshrc
#echo "entering ~/.zshrc"

# Uncomment in order to profile the Zsh startup time (needs to come at top of script)
# https://www.danielmoch.com/posts/2018/11/zsh-compinit-rtfm/#id5
# https://xebia.com/blog/profiling-zsh-shell-scripts/
#zmodload zsh/zprof
# (restart Zsh and then run "zprof" built-in command)
# Or, try timezsh(), timebash() in ~/.aliases

# NOTE ON 2018-11-09: This file needs to be cleaned up on 'master', since there are many
# remnants of comments from when Bash and Zsh were used in tandem that no longer apply.
# TODO: Add "export DEBUG_SHELL=1" toggle and "echo 'entering .zshrc, PATH='"
# debg statements that I have always manaully readded when issues occur (see NERSC dotfile manager)
#-----------------------
# bashrc is setup correctly. uncertain about relationship of zsh invoctation
# to the original bash login shell, and the optimal way to startup everything

# For example:
# The below is all repeated from .bashrc-- is there a better way to do this??
# Things get complicated when using multiple shells. Definitely need to repeat:
# - eval dircolors
# - source aliases and env

# what about ssh-agent? probably not since the daemon is launched in background
# could move to ssh-agent invocation to .bash_profile
# - confirmed that the agent remains running from .bashrc
# - confirmed that the modules remain loaded from .bashrc

# also, need to understand about sourcing global definitions in /etc/ for zsh
# Not really using the features of zsh anyway

##############################################################################
# 01. General and Oh-My-Zsh                                                  #
##############################################################################

# Override  $ZSH_OMZ/oh-my-zsh.sh explicit renaming of dumped compinit file
# Causes issues on remote clusters with head vs. compute node names resulting in
# many separate zcompdump files whenever an interactive shell/ scheduler job is launched
# .zcompdump-traverse-k01g10-5.0.2, .zcompdump-traverse-5.0.2, ..., etc.
#
# Assuming OS is not Darwin, shorten traverse.princeton.edu to traverse, e.g.
SHORT_HOST=${HOST/.*/}
#SHORTER_HOST=${SHORT_HOST/-*/}  # chop second half of "tiger-i19g10", e.g.
# might not work for ALCF machines, e.g. thetalogin5.alcf.anl.gov
if [ -z "$ZSH_COMPDUMP" ]; then
    #export ZSH_COMPDUMP="${ZDOTDIR:-${HOME}}/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"
    export ZSH_COMPDUMP="${ZDOTDIR:-${HOME}}/.zcompdump-remote-${ZSH_VERSION}"
fi

ZSH_DISABLE_COMPFIX="true"  # before sourcing oh-my-zsh.sh (saves 0.03 seconds)

# Override all locale settings in case of C/POSIX non-UTF8 language.
# Otherwise, remnant characters in prompt when deleting tab-completion in Zsh
export LC_ALL="en_US.UTF-8"

# Path to your oh-my-zsh installation. (Zsh itself is usually /bin/zsh on PICSciE clusters)
export ZSH=$HOME/dotfiles/oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    # internal/bundled plugins:
    git
    screen
    # external:
    conda-zsh-completion
    zsh-autosuggestions
    zsh-syntax-highlighting
)

# Source Oh-My-Zsh AFTER global defs
source $ZSH/oh-my-zsh.sh

# ls (GNU coreutils version) colors for Solarized dark
eval `dircolors ~/.dircolors.ansi-universal`

# For my extensive comments on Zsh command history configuration, see:
# https://github.com/felker/dotfiles_work/blob/master/zsh/config.zsh
#
# http://zsh.sourceforge.net/Doc/Release/Options.html#History
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

setopt HIST_VERIFY # Don't execute immediately upon history expansion
# e.g. "!r" + ENTER reloads the full line into the editing buffer but does not execute it
setopt EXTENDED_HISTORY # add Unix timestamps to history, format: ‘: <beginning time>:<elapsed seconds>;<command>’.
setopt no_share_history # do not share command history among GNU Screen windows (setopt is mutually exclusive with both of the following flags)
unsetopt APPEND_HISTORY # append rather than overwrite history file.
setopt INC_APPEND_HISTORY # prevent loss of command history entries due to unclean exit of the shell session
# Added in Zsh v5.0.6 (2014-08-29):
# setopt INC_APPEND_HISTORY_TIME  # (from the Zsh documentation) the history entry is immediately written out to the file AFTER the command is finished

# As of 2020-06-25, most /usr/bin/zsh available on my remote clusters are v5.0.2 (2012-12-21), probably since most use RHEL variants:
# Princeton: Traverse, Tiger 2, Adroit
# ANL: JLSE

# ALCF Theta uses v5.0.5 (2014-01-06); its OS is SUSE
# http://zsh.sourceforge.net/News/
# http://zsh.sourceforge.net/releases.html

setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt hist_expire_dups_first
setopt HIST_REDUCE_BLANKS
HISTORY_IGNORE="(ls|cd|pwd|exit|fc *|cd ..)"  # compare to Bash's HISTIGNORE. Added in v5.0.5
# this affects only history written to the history file, not the history in the currently active shell session's memory

##############################################################################
# 02. Aliases                                                                #
##############################################################################
# Already sourced in .bashrc, but aliases aren't inherited by child process
[ -f ~/.aliases ] && source ~/.aliases

##############################################################################
# 03. Environment variables                                                  #
##############################################################################
# (moved to .zlogin)
#[ -f ~/.env ] && source ~/.env

##############################################################################
# 04. Modules                                                                #
##############################################################################
# Only need to load in .bashrc-- they remain loaded even after exec zsh
# [ -f ~/.modules ] && source ~/.modules

##############################################################################
# 05. Other Zsh stuff                                                        #
##############################################################################
# Fix fpath on Linux clusters with Intel modules, necessary for TAB completion functions
# https://github.com/robbyrussell/oh-my-zsh/issues/4757
FPATH=/usr/share/zsh/5.0.2/functions:$FPATH
#echo $FPATH

# Fix delete key in zsh
# bindkey    "^[[3~"          delete-char
# bindkey    "^[3;5~"         delete-char
#

# See extensive comments in dotfiles_work.git zshrc
# ------------
# KGF: may not need to re-autoload and/or re-run Zsh completion system here
# No new completion functions added after OMZ is sourced?
#autoload -Uz compinit
#compinit -d ${ZSH_COMPDUMP}

# tab-completion for cd ..
zstyle ':completion:*' special-dirs true
# highlight tab selection from menu of possiblities, select with Enter
zstyle ':completion:*' menu select
# use ls-colors in tab completion menu
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"
zstyle ':completion::complete:*' use-cache 1
zstyle ":conda_zsh_completion:*" use-groups true

# Solarized Dark iTerm2 theme conflicts with the default autosuggestions font color, so only the first letter
# of the suggested command is visible. One-time fix (requires terminal emulator support for 256-color
# = 8 total bits over all 3x RGB channels):
#echo ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE=\'fg=60\' >> $ZSH_CUSTOM/zsh-autosuggestions_custom.zsh
# (executed in makesymlinks.sh)
# https://github.com/zsh-users/zsh-autosuggestions/issues/243

# KGF: workaround for running compinit in sourced config.zsh AFTER Oh-my-Zsh plugins
# are initialized. Need to re-source zsh-syntax-highlighting
source $ZSH/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Start ssh-agent daemon process upon login, if one isnt running
# Not currently using a -t timeout for the loaded keys

#debug_ssh_agent=1  # set to enable echo statements for debugging dotfiles interaction with SSH Agent

export SSH_ENV=$HOME/.ssh-agent-${SHORT_HOST}
# KGF: above is an important extension of all of the suggestions on StackOverflow I designed for the compute vs. head node problems on clusters
# The issue: interactive Slurm jobs, e.g., would launch a login Zsh on a compute node, which typically shares the $HOME filesystem
# with the head node. The active SSH Agent would be live on the head node, and have its connection info stored in ~/.ssh-agent, which would be
# overwritten by the Zsh on the compute node launching its own SSH Agent. This would orphan the original agent upon next SSH login to the headnode.

#ssh-add -l
#echo "ssh-add -l return code = $?"
ssh-add -l &>/dev/null # may echo "Could not open a connection to your authentication agent." to stdout or stderr
# "The authentication agent must be running and the SSH_AUTH_SOCK environment variable must contain the name of its socket for ssh-add to work."

if [ "$?" = 2 ]; then  # Use '=' instead of '==' for testing equality in Zsh
    # Could not open a connection to your authentication agent.--- logging in via SSH sholuld always enter this branch
    # since the relevant environment variables (${SSH_AUTH_SOCK}) are unset, even though an active SSH Agent is still running on the head node
    if [ ! -z "$debug_ssh_agent" ]; then
	echo "Failed to connect to SSH Agent, .zshrc now trying to refresh stored environment vars from ${SSH_ENV}"
	echo "before SSH_AGENT_PID=${SSH_AGENT_PID} SSH_AUTH_SOCK=${SSH_AUTH_SOCK}"
    fi
    # Load stored agent connection info.
    test -r ${SSH_ENV} && \
	eval "$(<${SSH_ENV})" >/dev/null
    if [ ! -z "$debug_ssh_agent" ]; then
	echo "after SSH_AGENT_PID=${SSH_AGENT_PID} SSH_AUTH_SOCK=${SSH_AUTH_SOCK}"
	echo "Checking connection to SSH agent again"
    fi
    ssh-add -l &>/dev/null
    if [ "$?" = 2 ]; then
	[ -z "$debug_ssh_agent" ] && echo "Failed to connect again... starting fresh agent"
	# Start agent and store agent connection info.
	(umask 066; ssh-agent > ${SSH_ENV})
	eval "$(<${SSH_ENV})" >/dev/null
	[ ! -z "$debug_ssh_agent" ] && echo "Started new agent (PID ${SSH_AGENT_PID}) and stored agent connection info in ${SSH_ENV}"
    else
	[ ! -z "$debug_ssh_agent" ] && echo "Successful connection on second attempt"
    fi
fi

# Load identities
# KGF: sometimes my SSH agent would be spawned, but I fail to load an identity
# Then, the above conditional would be insufficient, and the subsequent login would not prompt
# me for an SSH passphrase / try to load the identity.
ssh-add -l &>/dev/null
if [ "$?" = 1 ]; then
    [ ! -z "$debug_ssh_agent" ] && echo "adding key to SSH agent on PID ${SSH_AGENT_PID}"
    # The agent has no identities.
    # Time to add one.
    ssh-add #-t 4h
fi
# https://stackoverflow.com/questions/40549332/how-to-check-if-ssh-agent-is-already-running-in-bash

# How to check SSH Agent status and loaded keys:
# -----
# ssh-add -l
# echo $SSH_AGENT_PID $SSH_AUTH_SOCK
# ps -x | grep ssh-agent

# "ssh-agent" does the same thing as "ssh-agent -s": starts the agent running in the background AND
# generates Bash commands meant to be used in login shells as `eval ssh-agent -s`
# (which set environment variables)

# "ssh-agent -k" kills the currently running agent, identified by SSH_AGENT_PID
# so you have to keep changing the environment variable and running the kill command if you have multiple
# agents running that you want to terminate

# "killall ssh-agent" stopped working for me on Traverse in September 2020 ("no process found")
# "pgrep ssh-agent" and "pkill ssh-agent" still work
# https://unix.stackexchange.com/questions/14479/killall-gives-me-no-process-found-but-ps

# https://unix.stackexchange.com/questions/106847/what-does-aux-mean-in-ps-aux
# ps aux | grep kfelker
# a = show processes for all users
# u = display the process's user/owner
# x = also show processes not attached to a terminal

# TODO: consider adding some check to prevent interactive Slurm jobs on compute nodes (which spawn an interactive, login Zsh)
# from starting an ssh-agent on the compute node and prompting me for the SSH identity passphrase

# Check if $SLURM_* environment variables are set? None are set on login node. This approach would be specific to Slurm-based clusters,
# would not fix this problem for direct ssh onto compute nodes, and may not be desired anyway (e.g. if I want to "git pull" in a job)



# See "conda init --dry-run --verbose zsh" for how this next section is generated:

# Earlier versions of conda introduced scripts to make activation behavior uniform across operating systems.
# Conda 4.4 allowed conda activate myenv.
# Conda 4.6 added extensive initialization support so that conda works faster and less disruptively on a wide variety of shells (bash, zsh, csh, fish, xonsh, and more).
# Now these shells can use the conda activate command. Removing the need to modify PATH makes conda less disruptive to other software on your system

# Workaround for broken (for Zsh) conda shell function definitions in Princeton anaconda3 modulefiles
# for RHEL7 in June 2020:
# eval "$("$(which conda)" shell.zsh hook)”
# "module load anaconda3" after the below conda environment shell setup would break the "conda" shell function.
# Fixed by new version of Environment Modules and modulefiles after RHEL8 upgrade in Sept 2020.

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$('/usr/licensed/anaconda3/2020.2/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
if [ 1 -eq 0 ]; then
    # echo "kgf"  # usually enters this branch--- now skipping it entirely
    eval "$__conda_setup"
else  # commented out the following on generic master branch. Replace with actual module info on specific cluster
    # if [ -f "/usr/licensed/anaconda3/2020.2/etc/profile.d/conda.sh" ]; then
    #     . "/usr/licensed/anaconda3/2020.2/etc/profile.d/conda.sh"
    # else
    #     export PATH="/usr/licensed/anaconda3/2020.2/bin:$PATH"
    # fi
fi
#unset __conda_setup
# <<< conda initialize <<<

# https://github.com/conda/conda/issues/7855
# A TL;DR as this is currently the first hit on Google: the problem is that the
# NotImplementedError() raised in conda/activate.py takes a long time to pop up in some
# configurations. The quick-fix is to comment out the line that sets __conda_setup and
# forcing the if statement to fail immediately (replace $? -eq 0 with 1 -eq 0 or just
# replace the whole thing with what's in the else block). This bypasses the whole thing and
# uses the fall-back method of setting up your environment (which takes almost no time at
# all).

# KGF: but the side effect is that the base environment is not activated automatically and
# "(base)" no longer appears in the shell mode line, unless you also run (adding ~0.3s):
#conda activate

# -------------------------------------
# Vterm shell integration
# -------------------------------------
# These customizations could alternatively be found in
# ~/.emacs.d/elpa/vterm-20210105.349/etc/emacs-vterm-zsh.sh
# and sourced via:

# if [[ "$INSIDE_EMACS" = 'vterm' ]] \
#     && [[ -n ${EMACS_VTERM_PATH} ]] \
#     && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
# 	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
# fi

# KGF: TEMPORARY WORKAROUND UNTIL MY PR IN OH-MY-ZSH IS MERGED
export EMACS="*term*"
# My PR: https://github.com/ohmyzsh/ohmyzsh/pull/9577

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# for vterm-clear-scrollback, bound to C-c C-l
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# directory and prompt tracking
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

# See comments in .env; GNU Screen launches .zshrc, but not .zlogin.
# By default, tmux launches a login shell, but I have disabled this in tmux.conf
# --------
# Screen inherits most environment variables like PATH, EDITOR, etc.
# from the parent Zsh, but not LD_LIBRARY_PATH, LD_PRELOAD, etc.
#
# This is because /usr/bin/screen has the setgid mode bit active:
# Access: (2755/-rwxr-sr-x)  Uid: (    0/    root)   Gid: (   84/  screen)

# https://bugzilla.redhat.com/show_bug.cgi?id=129682
# if a user has set the LD_LIBRARY_PATH environment variable, and invokes a setuid/setgid program, that program gets the
# user's environment with LD_LIBRARY_PATH omitted, unless the user is the owner of the program file.
# This is for secuirty reasons.

# So, modify these env variables here instead of in ~/.env so that GNU Screen will rerun these lines
# However, ~/bin/tmux will also run these lines AND inherit the old LD_LIBRARY_PATH, resulting in duplicate entries
# "chmod g+s ~/bin/tmux" will not make tmux behave like screen / prevent inheriting LD* variables, since I am owner of this executable
export LD_LIBRARY_PATH=$HOME/lib/:$LD_LIBRARY_PATH
# this path modification is needed for built-from-source libevent for my tmux installation,
# + PostgreSQL and Boost installs for DeepHyper on Traverse


# LXterminal on Raspberry Pi OS does not have an option to run each window or tab's shell as a login shell:
# https://www.raspberrypi.org/forums/viewtopic.php?t=53408

#zsh -l
#lxterminal -l
####[ -f ~/.zlogin ] && source ~/.zlogin  # KGF: results in duplicate PATH entries on subsequent child (actuall non-login) shells

# Fix: Raspberry Pi Menu > Accessories > Terminal > Right click, Properties
# Tab: change "lxterminal" to "lxterminal -l"
