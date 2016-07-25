activation
activate
	"Initialize a delivery process."
	^ mutex critical: [ 
			self offerPermission.
			self isInactive ifTrue: [ self initializeProcess ].
			process ]