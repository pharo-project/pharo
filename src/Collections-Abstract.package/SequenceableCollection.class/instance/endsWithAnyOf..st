testing
endsWithAnyOf: aCollection
	"Return true if the receiver ends with any of the elements in aCollection."
	^aCollection anySatisfy:[:suffix| self endsWith: suffix]