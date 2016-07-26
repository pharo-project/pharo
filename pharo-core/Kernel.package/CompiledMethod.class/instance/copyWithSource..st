source code management
copyWithSource: aString
	^self copyWithTrailerBytes: (CompiledMethodTrailer new sourceCode: aString) 