access
indexOf: anElement ifAbsent: anExceptionBlock
	"If there are integers r, c such that (self at: r at: c) = anElement, answer some such r@c, otherwise answer the result of anExceptionBlock."

	^self rowAndColumnForIndex:
		 (contents indexOf: anElement ifAbsent: [^ anExceptionBlock value])
