initialize-release
initialize
	super initialize.
	updateActions := OrderedCollection new.
	portChangeActions := OrderedCollection new.
	rawSelectionTransmissions := IdentitySet new.
	self addRawSelectionTransmissionTo: #selection