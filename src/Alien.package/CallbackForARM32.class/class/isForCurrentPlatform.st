class initialization
isForCurrentPlatform
	| processor |
	processor := (SmalltalkImage current getSystemAttribute: 1003) asLowercase.
	^processor = 'arm' or: [(processor beginsWith: 'armv') and: [processor fifth <= $7]]