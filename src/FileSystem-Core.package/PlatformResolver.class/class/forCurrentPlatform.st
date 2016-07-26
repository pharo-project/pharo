instance creation
forCurrentPlatform
	| platformName |
	platformName :=  Smalltalk os platformName.
	^ (self allSubclasses detect: [:ea | ea platformName = platformName]) new