testing
supportsDiff

	^ assertionContext 
		ifNil: [ false ]
		ifNotNil: [ assertionContext selector = #assert:equals: ]