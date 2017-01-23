accessing
store
	"Answer the store I wrap"

	(currentStore isNil or: [ self needsReset ]) 
		ifTrue: [ self resetWithNextStoreName ].
	
	^ currentStore