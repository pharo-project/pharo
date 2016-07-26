activation
activate
	"Activate the owner too."
	
	self activatedModalChild ifTrue: [ ^self ].
	
	self addPaneSplittersIfNeeded.
		
	super activate.
	
	self isEmbedded ifFalse: [
		self basicActivate
	].	
	
	self navigateFocus.
	
	self positionModalOwner.