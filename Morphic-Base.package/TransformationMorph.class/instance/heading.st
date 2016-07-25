geometry etoy
heading
	"End recusion when necessary."
	| rendee |
	^ (rendee := self renderedMorph) == self 
		ifTrue: [ 0.0 ]
		ifFalse: [rendee heading]