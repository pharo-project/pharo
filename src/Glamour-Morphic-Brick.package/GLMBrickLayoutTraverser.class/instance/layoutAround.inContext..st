traversing
layoutAround: aBrick inContext: aContextBrick
	"want to layout non-Brick object? nice try, bro."
	aBrick isBrick ifFalse: [ ^ self ].
	"if my dimension is clean why do you want to layout me?"
	(self isClean: aBrick) ifTrue: [ ^ self ].
	aBrick changed.
	"custom dependencies are very dangerous, as it's complicated
	to detect cycles fast"
	(self customDependencies: aBrick)
		do: [ :each | |b|
			[ b :=  (each brickValue: aBrick) brickApi ] on: Exception do: [ b := nil ] .
			b ifNotNil: [ self layoutAround: b inContext: aBrick ] ].
		
	(self stateOf: aBrick) layout: aBrick on: self.
	
	"and finally we position them"
	self layoutSubbricks: aBrick.
	self layoutSubbricks: aBrick owner.
	
	aBrick changed.
