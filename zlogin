# ~/.zlogin
#echo "entering ~/.zlogin"
# Similar to .zprofile, but sourced only for login shells AFTER .zshrc
# (like .bash_login; I should prepend to PATH here for most interactive shells)

[ -f ~/.env ] && source ~/.env
