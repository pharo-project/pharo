multi-selection
mouseMoveOnSingle: evt
	"The mouse has moved with a button down.
	Do nothing if disabled."

	|row|
	self enabled ifFalse: [^self].
	row := self rowAtLocation: evt position.
	evt hand hasSubmorphs ifFalse: [
		((self containsPoint: evt position) and: [ row ~= 0 ]) 
			ifTrue: [self mouseDownRow: row]
			ifFalse: [self mouseDownRow: nil]].
	(self dropEnabled and:[evt hand hasSubmorphs]) 
		ifFalse: [^self eventHandler ifNotNil:
				[self eventHandler mouseMove: evt fromMorph: self]].
	(self containsPoint: evt position) 
		ifTrue: [self mouseEnterDragging: evt]
		ifFalse: [self mouseLeaveDragging: evt]