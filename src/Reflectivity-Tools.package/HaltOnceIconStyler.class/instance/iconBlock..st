defaults
iconBlock: aNode
	^[	aNode removeProperty: #Once ifAbsent: [aNode propertyAt: #Once put: true ]]