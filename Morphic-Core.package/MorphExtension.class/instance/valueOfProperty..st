accessing - other properties
valueOfProperty: aSymbol 
"answer the value of the receiver's property named aSymbol"
	^ self
		valueOfProperty: aSymbol
		ifAbsent: []