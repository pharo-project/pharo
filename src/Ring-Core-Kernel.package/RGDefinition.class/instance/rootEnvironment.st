accessing
rootEnvironment
	"Retrieves the runtime environment of an RGObject"
	"If the receiver is defined in an RGContainer the runtime environment is the one associated to its container"
	| parent |
	
	parent:= self environment.
	[ parent isRingObject ]
		whileTrue:[ parent:= parent environment ].

	^parent