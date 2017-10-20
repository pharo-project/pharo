instance creation
major: major minor: minor suffix: aString 
	"Create a new instance of the receiver with the given major minor number and suffix.

	 	SystemVersion major: 6 minor: 1 suffix: 'alpha'     
	"
	
	^self major: major minor: minor suffix: aString build: self invalidBuildNumber  