accessing
store
	"Answer the store I wrap"

	currentStore ifNil: [ ^ currentStore := self newStore ].
	
	self needsReset ifTrue: [ 
		self reset.
		
		currentStore := self newStore ].
	
	^ currentStore