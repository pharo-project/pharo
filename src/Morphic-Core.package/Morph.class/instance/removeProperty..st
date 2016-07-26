accessing - properties
removeProperty: aSymbol 
	"removes the property named aSymbol if it exists"
	extension ifNil:  [^ self].
	extension removeProperty: aSymbol