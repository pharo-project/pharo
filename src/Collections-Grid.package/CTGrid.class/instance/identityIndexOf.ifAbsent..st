access
identityIndexOf: anElement ifAbsent: anExceptionBlock
	^self rowAndColumnForIndex:
		 (contents identityIndexOf: anElement ifAbsent: [^anExceptionBlock value])
