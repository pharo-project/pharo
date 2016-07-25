defaults
iconLabelBlock: aNode
	^ [(aNode hasProperty: #Once) 
		ifTrue: ['inactive, click to turn on']
		ifFalse: ['active, click to turn off' ]]