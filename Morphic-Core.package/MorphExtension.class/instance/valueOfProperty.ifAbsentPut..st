accessing - other properties
valueOfProperty: aSymbol ifAbsentPut: aBlock 
	"If the receiver possesses a property of the given name, answer  
	its value. If not, then create a property of the given name, give 
	it the value obtained by evaluating aBlock, then answer that  
	value"
	^self assureOtherProperties at: aSymbol ifAbsentPut: aBlock