instance variables
classThatDefinesClassVariable: classVarName
	"Answer the class that defines the given class variable"

	(self classPool includesKey: classVarName asSymbol) ifTrue: [^ self]. 
	^self superclass ifNotNil: [self superclass classThatDefinesClassVariable: classVarName]