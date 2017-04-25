misc
loadLibrary: path flag: flag
	^ self forCurrentPlatform 
		loadLibrary: path 
		flag: flag