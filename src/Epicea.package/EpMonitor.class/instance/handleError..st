private
handleError: error

	self isDebug
		ifFalse: [ self inform: error asString ]
		ifTrue: [ error signal ]