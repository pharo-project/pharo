Usage: update [ --force ] [ --from-file=<updateFile> ]
	--force        Continue loading updates with errors
	--from-file    Use a local <updateFile> instead of http://updates.pharo.org
	<updateFile>   An update file containing a cr-separated list of urls to .cs files
	
Documentation:
	The update the image to the latest version.

Usage:
	pharo Pharo.image update
	pharo Pharo.image update --from-file=udpates.list