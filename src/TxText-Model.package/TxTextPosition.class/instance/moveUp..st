navigation
moveUp: number
	| oldLinePosition  pos |
	
	(number < 0)
		ifTrue: [ ^ self moveDown: number negated ].
	
	oldLinePosition := self linePosition.
	pos := self.
	
	number timesRepeat: [ 
		pos := pos moveToLineStart moveLeft.
		pos isAtStart ifTrue: [  ^ pos ]
	].

	^ pos linePosition: oldLinePosition.
