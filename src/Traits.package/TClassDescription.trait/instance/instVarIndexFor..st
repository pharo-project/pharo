instance variables
instVarIndexFor: instVarName
	"Answer the index of the named instance variable."

	^self instVarIndexFor: instVarName ifAbsent: 0