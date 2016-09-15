accessing
store
	"Answer the store I wrap"

	(currentStore isNil or: [ self needsReset ]) ifTrue: [ 
		self resetWithStoreNamed: self randomName ].
	
	^ currentStore