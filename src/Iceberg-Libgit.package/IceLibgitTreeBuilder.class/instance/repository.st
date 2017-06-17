accessing
repository
	^ parent 
		ifNotNil: [ self parent repository ]
		ifNil: [ self initialTree owner ]