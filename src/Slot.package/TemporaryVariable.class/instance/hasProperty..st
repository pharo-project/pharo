properties
hasProperty: aKey
	"Test if the property aKey is present."
	^self properties 
		ifNil: [false]
		ifNotNil: [:prop | prop includesKey: aKey]