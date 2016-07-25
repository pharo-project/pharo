defaults
iconLabelBlock: aNode
	^ [(aNode hasProperty: #haltCount) 
		ifTrue: [(aNode propertyAt: #haltCount) asString, ' ,click to reset']
		ifFalse: ['count: 0' ]]