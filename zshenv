# ~/.zshenv

# Source global definitions, even for non-interactive .zsh scripts
# Unlike BASH_ENV, this is also sourced for interactive Zsh shells
if [ -f /etc/zshrc ]; then
	. /etc/zshrc
fi

# Set so that non-interactive Bash scripts get essential global definitions
# when executed in Zsh. See this file for details
export BASH_ENV=$HOME/.bashenv
# (already exported in ~/.env via ~/.zshrc for interactive Zsh shells)
