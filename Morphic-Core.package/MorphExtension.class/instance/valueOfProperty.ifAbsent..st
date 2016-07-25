accessing - other properties
valueOfProperty: aSymbol ifAbsent: aBlock 
	"if the receiver possesses a property of the given name, answer  
	its value. If not then evaluate aBlock and answer the result of  
	this block evaluation"
	otherProperties ifNil: [^ aBlock value].
	^ otherProperties at: aSymbol ifAbsent: [^ aBlock value]