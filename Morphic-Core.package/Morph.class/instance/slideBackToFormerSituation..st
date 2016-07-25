dropping/grabbing
slideBackToFormerSituation: evt 
	| slideForm formerOwner formerPosition aWorld startPoint endPoint trans |
	formerOwner := self formerOwner.
	formerPosition := self formerPosition.
	aWorld := evt hand world.
	trans := formerOwner transformFromWorld.
	slideForm := trans isPureTranslation 
				ifTrue: [self imageForm offset: 0 @ 0]
				ifFalse: 
					[((TransformationMorph new asFlexOf: self) transform: trans) imageForm 
						offset: 0 @ 0]. 
	startPoint := evt hand fullBounds origin.
	endPoint := trans localPointToGlobal: formerPosition.
	owner removeMorph: self.
	aWorld displayWorld.
	slideForm 
		slideFrom: startPoint
		to: endPoint
		nSteps: 12
		delay: 15.
	formerOwner addMorph: self.
	self position: formerPosition.
	self justDroppedInto: formerOwner event: evt