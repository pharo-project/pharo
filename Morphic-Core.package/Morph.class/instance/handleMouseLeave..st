events-processing
handleMouseLeave: anEvent
	"System level event handling."
	anEvent hand removePendingBalloonFor: self.
	anEvent isDraggingEvent ifTrue:[
		(self handlesMouseOverDragging: anEvent) ifTrue:[
			anEvent wasHandled: true.
			self mouseLeaveDragging: anEvent].
		^ self eventHandler ifNotNil: [:handler | handler mouseLeave: anEvent fromMorph: self ]].
	(self handlesMouseOver: anEvent) ifTrue:[
		anEvent wasHandled: true.
		self mouseLeave: anEvent ].
	^ self eventHandler ifNotNil: [:handler | handler mouseLeave: anEvent fromMorph: self ]