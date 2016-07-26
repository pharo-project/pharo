accessing
numArgs
	"Answer the number of arguments for this activation."
	^closureOrNil
		ifNil: [method numArgs]
		ifNotNil: [closureOrNil numArgs]