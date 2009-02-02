#!/bin/bash
# 
#   imageshack_upload.sh 
#  
#   Copyright (c) 2007 by enki <enki@crocobox.org>
#  
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#  
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
#   USA.
#
#   Changelog :
#   * 2006/10/22 -	Updated to support files with space . 
#			      Changes proposed by slubman(http://www.slubman.net)
#   * 2006/10/22 -	First version
myver='0.2'
CURL=$(which curl)
TMPFILE=$(mktemp /tmp/imageshack.XXXXXXXXXX) || exit 1
cleanup() {
        rm -rf $TMPFILE
        [ "$1" ] && exit $1
}
usage() {
	echo "imageshack_upload $myver"
	echo "usage: $0 <root>"
	echo
	echo "This script allow you to send image to htpp://www.imageshack.us"
	echo "in command line"
	echo
	echo "example: imageshack_upload.sh `pwd`/myimage.png" 
	echo
}

if [ $# -lt 1 ]; then
	usage
	rm -rf $TMPFILE
	exit 1
fi
if [ "$1" = "-h" -o "$1" = "--help" ]; then
	usage
	rm -rf $TMPFILE
	exit 0
fi
img="$1"
if ! [ -f "$img" ]; then
	echo "Error: file don't exist"
	exit 1
fi
$CURL -H Expect: -F fileupload="@${img}" -F xml=yes http://www.imageshack.us/index.php > $TMPFILE
echo "Url of image on imageshack:"
echo $(cat $TMPFILE | grep -E "<image_link>(.*)</image_link>" | sed 's|<image_link>\(.*\)</image_link>|\1|')
cleanup
