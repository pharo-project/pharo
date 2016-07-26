accessing - other properties
removeProperty: aSymbol 
	"removes the property named aSymbol if it exists"
	otherProperties ifNil: [^ self].
	otherProperties removeKey: aSymbol ifAbsent: [].
	otherProperties isEmpty ifTrue: [self removeOtherProperties]