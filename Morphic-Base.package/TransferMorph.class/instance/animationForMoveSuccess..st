dropping/grabbing
animationForMoveSuccess: success 
	| start stop slideForm |
	success
		ifTrue: [^ self]
		ifFalse: [
			start := self fullBounds origin.
			stop := self formerPosition ifNil: [ self source bounds origin ]].
	start = stop ifTrue: [^ self].
	
	slideForm := self imageForm offset: 0@0.
	slideForm
		slideWithFirstFrom: start
		to: stop
		nSteps: 12
		delay: 20