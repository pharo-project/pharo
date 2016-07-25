accessing - other properties
setProperty: aSymbol toValue: abObject 
	"change the receiver's property named aSymbol to anObject"
	self assureOtherProperties at: aSymbol put: abObject