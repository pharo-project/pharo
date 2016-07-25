accessing
typesFor: anInstVarName in: aClass 
	class = aClass ifFalse: [self runOn: aClass].
	^variableTypes at: anInstVarName ifAbsent: [Set new]