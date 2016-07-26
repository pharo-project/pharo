testing
looksLikeDirectory
	^fileName last = $/
		and: [ uncompressedSize = 0 ]