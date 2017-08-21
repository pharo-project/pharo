instance variables
classThatDefinesInstanceVariable: instVarName
	(self instVarNames notNil and: [self instVarNames includes: instVarName asString]) ifTrue: [^ self]. 
	^self superclass ifNotNil: [self superclass classThatDefinesInstanceVariable: instVarName]