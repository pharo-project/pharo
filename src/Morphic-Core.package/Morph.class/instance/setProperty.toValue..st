accessing - properties
setProperty: aSymbol toValue: anObject 
	"change the receiver's property named aSymbol to anObject"
	anObject ifNil: [^ self removeProperty: aSymbol].
	self assureExtension setProperty: aSymbol toValue: anObject