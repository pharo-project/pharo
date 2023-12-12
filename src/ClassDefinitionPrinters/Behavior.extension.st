Extension { #name : 'Behavior' }

{ #category : '*ClassDefinitionPrinters' }
Behavior >> kindOfSubclass [
	"Answer a String that is the keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton, see #subclassDefiningSymbol"
	<reflection: 'Class structural inspection - Iterating and querying hierarchy'>
	^ self classLayout kindOfSubclass
]
