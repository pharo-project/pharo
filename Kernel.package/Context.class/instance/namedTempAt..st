debugger access
namedTempAt: index
	"Answer the value of the temp at index in the receiver's sequence of tempNames."
	^self debuggerMap namedTempAt: index in: self