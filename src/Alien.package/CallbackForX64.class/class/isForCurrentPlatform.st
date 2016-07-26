class initialization
isForCurrentPlatform
	| processor |
	CallbackForWin64X64 isForCurrentPlatform ifTrue:
		[^false].
	processor := (SmalltalkImage current getSystemAttribute: 1003) asLowercase.
	^processor first = $x and: [processor = 'x64' or: [('x86#64*' match: processor)]]