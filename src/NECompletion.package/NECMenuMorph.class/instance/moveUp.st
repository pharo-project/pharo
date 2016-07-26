actions
moveUp
	(self selected = 0
			and: [self firstVisible = 1])
		ifTrue: [^ self].
	self selected: self selected - 1.
	self selected < self firstVisible
		ifTrue: [firstVisible := firstVisible - 1].
	self changed. 