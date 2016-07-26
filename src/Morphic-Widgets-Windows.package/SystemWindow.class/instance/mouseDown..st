events
mouseDown: evt
	"Changed to properly process the mouse down event if passing to
	submorphs."
		
	self isEmbedded ifTrue: [^self].
	
	self isTopWindow
		ifTrue: [self comeToFront] "rise above non-window morphs"
		ifFalse:[	self activate].

	evt hand waitForClicksOrDrag: self event: evt.  "allow double-click/drag response"
	
	evt wasHandled: false.
	self submorphsDo: [ :morph | "but allow a submorph to process and override double-click/drag response."
		(morph containsPoint: evt cursorPoint) ifTrue: [ morph processEvent: evt ]
	].
	evt wasHandled: true.
	
	