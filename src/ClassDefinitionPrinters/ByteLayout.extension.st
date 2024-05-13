Extension { #name : 'ByteLayout' }

{ #category : '*ClassDefinitionPrinters' }
ByteLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	^#variableByteSubclass:
]
