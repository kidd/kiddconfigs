#!/bin/bash
#===============================================================================
#
#          FILE:  rat-volume.sh
# 
#         USAGE:  ./rat-volume.sh 
# 
#   DESCRIPTION:  
# 
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#          BUGS:  ---
#         NOTES:  ---
#        AUTHOR:   (), 
#       COMPANY:  
#       VERSION:  1.0
#       CREATED:  10/11/07 20:46:19 CET
#      REVISION:  ---
#===============================================================================

# rat-volume by Stephan Walter <stephan@walter.name>, License: BSD

amixer set Master $1

ratpoison -c "echo `amixer get Master \
| grep Mono: \
| sed 's/^.*\[\(.*\)%\].*/\1/' \
| awk '{for(i=0;i<\$1/3;i++)printf \"#\"; for(;i<100/3;i++)printf \" \"}'`"
ratpoison -c "set topkmap vol"
