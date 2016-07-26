accessing - properties
valueOfProperty: aSymbol 
	"answer the value of the receiver's property named aSymbol"
	^ extension ifNotNil: [extension valueOfProperty: aSymbol]