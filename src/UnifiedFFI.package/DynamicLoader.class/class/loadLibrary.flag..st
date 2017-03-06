misc
loadLibrary: path flag: flag
	^ self uniqueInstance 
		loadLibrary: path 
		flag: flag