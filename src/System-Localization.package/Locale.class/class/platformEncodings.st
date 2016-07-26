accessing
platformEncodings
	PlatformEncodings isEmptyOrNil ifTrue: [ self initializePlatformEncodings ].
	^PlatformEncodings
