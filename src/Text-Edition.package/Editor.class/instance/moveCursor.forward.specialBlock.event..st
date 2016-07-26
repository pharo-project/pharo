private
moveCursor: directionBlock forward: forward specialBlock: specialBlock event: aKeyboardEvent
	"Private - Move cursor.
	directionBlock is a one argument Block that computes the new Position from a given one.
	specialBlock is a one argumentBlock that computes the new position from a given one under the alternate semantics.
	Note that directionBlock always is evaluated first."
	| shift indices newPosition |
	self morph manageCursor.
	shift := aKeyboardEvent shiftPressed.
	indices := self setIndices: shift forward: forward.
	newPosition := directionBlock value: (indices at: #moving).
	(aKeyboardEvent commandKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [newPosition := specialBlock value: newPosition].
	shift
		ifTrue: [self selectMark: (indices at: #fixed) point: newPosition - 1]
		ifFalse: [self selectAt: newPosition]