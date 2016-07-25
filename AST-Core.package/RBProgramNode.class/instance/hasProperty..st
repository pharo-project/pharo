properties
hasProperty: aKey
	"Test if the property aKey is present."
	
	^ properties notNil and: [ properties includesKey: aKey ]