accessing
forwardDirection
	"Return the rendee's forward direction. 
	If I have no rendee then return 0.0 degrees "
	| rendee |
	^ ( rendee := self renderedMorph) == self  
		ifTrue: [0.0 ]
		ifFalse: [^ rendee forwardDirection]