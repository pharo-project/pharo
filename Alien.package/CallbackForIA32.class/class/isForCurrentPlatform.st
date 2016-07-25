class initialization
isForCurrentPlatform
	| processor |
	processor := (SmalltalkImage current getSystemAttribute: 1003) asLowercase.
	^processor first = $i and: [processor = 'intel' or: ['i#86' match: processor]]