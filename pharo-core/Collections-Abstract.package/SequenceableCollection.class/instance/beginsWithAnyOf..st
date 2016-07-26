testing
beginsWithAnyOf: aCollection
	"Return true if the receiver starts with any of the elements in aCollection."
	^aCollection anySatisfy:[:prefix| self beginsWith: prefix]