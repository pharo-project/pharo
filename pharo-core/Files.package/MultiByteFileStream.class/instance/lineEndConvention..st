accessing
lineEndConvention: aSymbol

	(lineEndConvention := aSymbol) ifNotNil: [wantsLineEndConversion := true].
	self installLineEndConventionInConverter