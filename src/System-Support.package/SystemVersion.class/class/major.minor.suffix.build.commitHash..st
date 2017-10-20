instance creation
major: major minor: minor suffix: aString build: build commitHash: hashString
	"Create a new instance of the receiver with the given major minor number and suffix.

	 	SystemVersion major: 6 minor: 1 suffix: 'alpha'     
	"
	
	^(self new)
		major: major;
		minor: minor;
		suffix: aString;
		build: build;
		commitHash: hashString;
		yourself