stepping and presenter
step
	"Move the more column if it is showing."

	self moreVisible ifTrue: [
		self window ifNotNil: [:w |
			w isActive ifFalse: [self hideMore]].
		self moreMorph position = self desiredMoreMorphPosition
			ifFalse: [self hideMore]]