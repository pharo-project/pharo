accessing
numTemps
	"Answer the number of temporaries for the receiver; this includes
	 the number of arguments and the number of copied values."
	^self numCopiedValues
	 + self numArgs
	 + self numLocalTemps