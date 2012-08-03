capture-mylife.el
==================
Capture your life from Emacs.
capture-mylife.el is inspired by [this article](http://d.hatena.ne.jp/nishiohirokazu/20120731/1343745529)


Requirements
------------
* Emacs 22.1 or higher.
* *scrot*, *import*(Linux), *screencapture*(MacOSX), *nircmd.exe*(Windows)
* *avconv* or *ffmpeg*

MacOSX and Windows are not tested yet.


Basic Usage
-----------

Start capturing your life. You can select desktop' or 'active',
but 'active' is supported only Linux where is installed *scrot*.

    M-x capture-mylife:start

Finish capturing your life.

    M-x capture-mylife:stop

Continue capturing your life. You can use this command after emacs crashed.

    M-x capture-mylife:continue

Convert movie from captured images.

    M-x capture-mylife:convert-movie
