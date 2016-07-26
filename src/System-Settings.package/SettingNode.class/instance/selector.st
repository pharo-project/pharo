accessing
selector
	^ self pragma 
		ifNil: ['']
		ifNotNil: [:p | p selector]