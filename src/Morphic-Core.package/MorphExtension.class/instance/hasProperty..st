accessing - other properties
hasProperty: aSymbol 
	"Answer whether the receiver has the property named aSymbol"
	| property |
	otherProperties ifNil: [^ false].
	property := otherProperties at: aSymbol ifAbsent: [].
	property ifNil: [^ false].
	property == false ifTrue: [^ false].
	^ true