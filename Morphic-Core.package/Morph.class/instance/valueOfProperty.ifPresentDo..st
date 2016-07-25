accessing - properties
valueOfProperty: aSymbol ifPresentDo: aBlock 
	"If the receiver has a property of the given name, evaluate  
	aBlock on behalf of the value of that property"
	extension ifNil:  [^ self].
	^ aBlock value: (extension valueOfProperty: aSymbol ifAbsent: [^ self])