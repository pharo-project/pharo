as yet unclassified
new: numberOfElements
	"Create an instance large enough to hold numberOfElements methods without growing."
	
	^self newForCapacity: (self sizeFor: numberOfElements)