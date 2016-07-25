scrolling
scrollDown: anInteger
	
	band brickBounds corner x <= self width ifTrue: [ ^ self ].
	band brickBounds: (band brickBounds translateBy: (anInteger@ 0) negated).
	band brickBounds corner x < self width ifTrue: [ band brickBounds: (band brickBounds translateBy: ((self width - band brickBounds corner x)@0)) ]