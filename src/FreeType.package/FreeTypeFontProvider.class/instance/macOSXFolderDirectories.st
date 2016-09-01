file paths
macOSXFolderDirectories
	| localUserFonts |
	"Answer the Mac OS X font folder paths.
	This needs some FFI code, but for the time being, we guess these and omit the user fonts folder"
	
	"Adding some directories following the guidelines in https://support.apple.com/en-us/HT201722"
	
	localUserFonts := FileLocator home / 'Library' / 'Fonts'.
	
	^{localUserFonts} , #('/System/Library/Fonts' '/Network/Library/Fonts/' '/Library/Fonts')
		collect: [ :each| each asFileReference ]
		thenSelect: [ :each| each exists ].