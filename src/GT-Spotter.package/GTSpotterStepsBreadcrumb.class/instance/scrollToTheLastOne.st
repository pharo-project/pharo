geometry
scrollToTheLastOne
	|leftDelta rightDelta delta|
	self items isEmpty ifTrue: [ ^ self ].
	
	rightDelta := self globalBounds right - self band globalBounds right.
	leftDelta := self globalBounds left - self band globalBounds left.
			
	"all visible, return"
	(leftDelta = 0  and: [ self items size <= self maxNumberOfSteps ]) ifTrue: [ ^ self ].
	
	delta := (self items size <= self maxNumberOfSteps) ifTrue: [ leftDelta ] ifFalse: [ rightDelta ].
	delta := self band brickBounds left + delta - self band data.
	
	self
		glmAnimateBrickBounds: delta
		duration: 400
		callback: nil.
	
	