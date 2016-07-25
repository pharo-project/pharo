comparing
< aPosition
	"Answer true if receiver is closer to start of the text than argument, false otherwise.
	Note, both positions should belong to same text"
		
	aPosition text == self text ifFalse: [ self error: 'Cannot compare positions belonging to different text' ].

	"local case first"
	self span == aPosition span ifTrue: [ 
		^ self spanPosition < aPosition spanPosition
	].
	
	^ self span < aPosition span