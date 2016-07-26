initialization
initialize
	super initialize.
	self changeProportionalLayout.
	self
		onAnnouncement: MorphLostFocus do: [ :ann | ann morph delete ];
		color: Color transparent