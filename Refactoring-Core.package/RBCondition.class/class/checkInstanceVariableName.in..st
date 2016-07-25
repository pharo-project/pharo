utilities
checkInstanceVariableName: aName in: aClass 
	| string |
	aName isString ifFalse: [^false].
	string := aName asString.
	string isEmpty ifTrue: [^false].
	(self reservedNames includes: string) ifTrue: [^false].
	string first isUppercase ifTrue: [^false].
	^RBScanner isVariable: string