command line
request: prompt 
	| startPos char contents |
	self cr; show: prompt.
	startPos := position.
	Sensor flushKeyboard.
	[(char := self nextChar) = Character cr]
		whileFalse: [char = Character backspace
				ifTrue: [readLimit := position := position - 1 max: startPos]
				ifFalse: [self nextPut: char].
			self endEntry].
	contents := self contents.
	^ contents copyFrom: startPos + 1 to: contents size 