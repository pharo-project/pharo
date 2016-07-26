accessing
numTemps
	"Answer the number of temporaries for this activation; this includes
	 the number of arguments, and for blocks, the number of copied values."
	^closureOrNil
		ifNil: [method numTemps]
		ifNotNil: [closureOrNil numTemps]