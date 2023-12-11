Extension { #name : 'CompiledMethodLayout' }

{ #category : '*ClassDefinitionPrinters' }
CompiledMethodLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, usage makes no sense for anything
	else!"
	"there is no way to define classes of this Layput, the system shows them as variable classes"
	^#variableByteSubclass:
]
