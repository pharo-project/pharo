utilities
checkClassVarName: aName in: aClass 
	| string |
	aName isString ifFalse: [^false].
	string := aName asString.
	(self reservedNames includes: string) ifTrue: [^false].
	string isEmpty ifTrue: [^false].
	string first isUppercase ifFalse: [^false].
	^RBScanner isVariable: string