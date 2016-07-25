*NautilusCommon
correspondingClass

	(self name endsWith: 'Test' ) ifFalse: [^nil].
	^Smalltalk globals 
		at: (self name allButLast: 4) asSymbol
		ifAbsent: [nil]