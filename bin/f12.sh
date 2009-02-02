#!/bin/sh

# ~/bin/f12

if [ "$(ratpoison -c info | grep screen)" ]; then
#if [ "$(ratpoison -c info | grep aterm)" ]; then

	# if we're currently in terminal, go back to the last non-terminal window
	ratpoison -c other

	if [ "$(ratpoison -c info | grep screen)" ]; then
#	if [ "$(ratpoison -c info | grep aterm)" ]; then

		# if we're still in terminal, we must have a split-frame, so focus the last non-terminal frame
		ratpoison -c focuslast

		if [ "$(ratpoison -c info | grep screen)" ]; then
#		if [ "$(ratpoison -c info | grep aterm)" ]; then
			# if we're *still* in terminal, it must be the only thing running, so hide it
			ratpoison -c "select -"
		fi
	fi

	exit 0
fi

if [ "$(ratpoison -c windows | grep screen)" ]; then
#if [ "$(ratpoison -c windows | grep aterm)" ]; then

	# switch to already-open terminal
	ratpoison -c "select screen"

	exit 0
fi

# start new terminal and immediately start screen within it
urxvt -T screen -e screen -RD &
#urxvt -T screen -tr -tint ivory -sh 10 -e screen -RD &

