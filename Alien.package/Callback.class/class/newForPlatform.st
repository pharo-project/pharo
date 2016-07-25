instance creation
newForPlatform
	"Answer an instance of the subclass for this platform's ABI, if any."
	^concreteClass
		ifNotNil: [:cc| cc new]
		ifNil: [self error: 'could not find a concrete Callback class for this platform']