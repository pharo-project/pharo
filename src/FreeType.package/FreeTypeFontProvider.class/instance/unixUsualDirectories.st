file paths
unixUsualDirectories
	"Trying with some common directories in unix. The prefered version is to read the /etc/fonts/fonts.conf"
	
	| localUserFonts |
	localUserFonts := FileLocator home / 'Library' / 'Fonts'.
	
	^ {localUserFonts} , #('/usr/share/fonts' '/usr/local/share/fonts') collect: [ :each | each asFileReference ] thenSelect: [ :each | each exists ]