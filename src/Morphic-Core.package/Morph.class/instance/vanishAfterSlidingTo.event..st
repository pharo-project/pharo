dropping/grabbing
vanishAfterSlidingTo: aPosition event: evt

	| aForm aWorld startPoint endPoint |
	aForm := self imageForm offset: 0@0.
	aWorld := self world.
	startPoint := evt hand fullBounds origin.
	self delete.
	aWorld displayWorld.
	endPoint := aPosition.
	aForm slideFrom: startPoint  to: endPoint nSteps: 12 delay: 15.