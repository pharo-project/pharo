accessing
errorBlock: aBlock 
	errorBlock := aBlock.
	scanner notNil ifTrue: [scanner errorBlock: aBlock]