controlling
ifFalse: alternativeBlock 
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."

	^alternativeBlock value