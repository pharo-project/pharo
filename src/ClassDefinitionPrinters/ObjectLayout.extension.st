Extension { #name : 'ObjectLayout' }

{ #category : '*ClassDefinitionPrinters' }
ObjectLayout >> kindOfSubclass [
	"Answer a String that is the keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, see #subclassDefiningSymbol"
	^' ',self class subclassDefiningSymbol, ' '
]

{ #category : '*ClassDefinitionPrinters' }
ObjectLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	"As a fallback we just return a standard class creation symbol. This will be called for user
	defined Layouts, for old style class definitions that can not support user defined Layouts"
	^#subclass:
]

{ #category : '*ClassDefinitionPrinters' }
ObjectLayout >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else as CompiledMethodLayout answers the same as ByteLayout and user defined Layouts are not
	supported"
	^self class subclassDefiningSymbol
]
