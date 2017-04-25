class initialization
isForCurrentPlatform
	| processor |
	(#('Win32' 'Win64') includes: (SmalltalkImage current getSystemAttribute: 1001)) ifFalse:
		[^false].
	processor := (SmalltalkImage current getSystemAttribute: 1003) asLowercase.
	^processor first = $x and: [processor = 'x64' or: [('x86#64*' match: processor)]]