validation
validate
	"If the receiver is not valid (has a nil handle), then create the 
	receiver to obtain a handle and load the receiver's fields"
	
	(session == Smalltalk session 
		and: [ self isValid 
		and: [ numCharmaps notNil ] ]) 
		ifTrue: [ ^ self ].
	
	session := Smalltalk session.
	handle := nil.
	self create