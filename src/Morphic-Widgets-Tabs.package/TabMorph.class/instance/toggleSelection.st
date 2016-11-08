private
toggleSelection

	self selected
		ifTrue: [ self removeFromSelection ]
		ifFalse: [ self addToSelection ]