private - 3 installing
installFreshChangesFile
	| changesFile |
	
	"install a new .changes file"
	changesFile := Smalltalk changesFile.
	changesFile moveTo: (changesFile , 'bak') nextVersion.
	
	"create a fresh changes file "
	changesFile writeStreamDo: [ :strm |
		 strm timeStamp ].