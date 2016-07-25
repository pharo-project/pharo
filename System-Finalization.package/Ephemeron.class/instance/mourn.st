mourning
mourn
	"The key is only referenced by myself.
	Finalize the key and remove myself from my container.
	
	This Ephemeron instance is not ephemeric anymore: it cannot be reused."

	key finalize.
	container ifNotNil: [ container removeEphemeron: self ].