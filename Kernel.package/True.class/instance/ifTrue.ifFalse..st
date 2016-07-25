controlling
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
	"Answer with the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value