Usage: clean [ --release ]
	--release        do #cleanUpForRelease
	--production   do #cleanUpForProduction
	
Documentation:
	This allows to run the ImageCleaner from the commandLine.
	With no special option it runs Smalltalk cleanUp: true.

Usage:
	pharo Pharo.image clean
	pharo Pharo.image clean --release