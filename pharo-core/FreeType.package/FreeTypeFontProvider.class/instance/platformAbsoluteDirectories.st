file paths
platformAbsoluteDirectories
	Smalltalk os isWin32
		ifTrue: [ ^ self winFontDirectories ].
	Smalltalk os isUnix
		ifTrue: [ ^ self unixFontDirectories ].
	Smalltalk os isMacOSX
		ifTrue: [ ^ self macOSXFolderDirectories ].
	^ {}