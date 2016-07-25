class initialization
isForCurrentPlatform
	| processor |
	(SmalltalkImage current getSystemAttribute: 1003) ~= 'Win64' ifTrue:
		[^false].
	processor := (SmalltalkImage current getSystemAttribute: 1003) asLowercase.
	^processor first = $x and: [processor = 'x64' or: [('x86#64*' match: processor)]]