# ~/.bashrc

##############################################################################
# 01. General                                                                #
##############################################################################

# ls (GNU coreutils version) colors for Solarized dark
eval `dircolors ~/.dircolors.ansi-universal`

# Start ssh-agent daemon process upon login, if one isnt running
# Not currently using a -t timeout for the loaded keys
# ssh-add -l &>/dev/null
# if [ "$?" == 2 ]; then
#   test -r ~/.ssh-agent && \
#     eval "$(<~/.ssh-agent)" >/dev/null

#   ssh-add -l &>/dev/null
#   if [ "$?" == 2 ]; then
#     (umask 066; ssh-agent > ~/.ssh-agent)
#     eval "$(<~/.ssh-agent)" >/dev/null
#     ssh-add
#   fi
# fi

##############################################################################
# 02. Aliases                                                                #
##############################################################################
[ -f ~/.aliases ] && source ~/.aliases

##############################################################################
# 03. Environment variables                                                  #
##############################################################################
[ -f ~/.env ] && source ~/.env

# Source essential global definitions
if [ -f $BASH_ENV ]; then
	. $BASH_ENV
fi

##############################################################################
# 04. Modules                                                                #
##############################################################################
# [ -f ~/.modules ] && source ~/.modules

##############################################################################
# 05. (Optional) Spawn Zsh                                                   #
##############################################################################
# "exec zsh"  command will overlay shell, inherting environment variables and
# shell functions marked for export, pwd, open files, etc. (including loaded
# modules in exported LOADEDMODULES env variable, etc).
# However, the shell aliases will NOT be inherited
# file, aliases.sh, and source it from both zshrc and bashrc?
# echo LOADEDMODULES

# Approach #1:
# These checks are needed since I am sourcing .bashrc from .bash_profile:
# if this is an interactive shell
# if [[ $- == *i* ]]; then
#   # if on one of those annoying hosts...
#   # User can define the pattern of hostnames so that this code snippet can be
#   # used in all .bashrc, but only `exec zsh` for the non chsh compliant
#   # hostnames, such as "tiger1", for example.
# #  if [[ `uname -n` == PATTERN_MATCHING_SOME_HOSTNAMES ]]; then
#      # if there is actually a zsh command available
#      if [[ -x `which --skip-alias zsh 2>/dev/null` ]]; then
#         # avoid spawning zsh every time bash is started...
#         if [ -z $ZSH_STARTED ]; then
#             export ZSH_STARTED="true"
#             # if the call to zsh fails, scp a different conf there
#             exec zsh
#         fi
#      fi
#  # fi
# fi

# Approach #2:
# export SHELL=`which zsh`
# -l flag is for login shell. Loads /etc/zprofile, which modifies $PATH on PICSciE
# [ -z "$ZSH_VERSION" ] && exec "$SHELL" #-l

# if [ -x /bin/zsh ]; then exec /bin/zsh; fi
