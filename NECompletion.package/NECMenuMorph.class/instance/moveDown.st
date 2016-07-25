actions
moveDown
	self selected: self selected + 1.
	(self selected > self lastVisible
			and: [self selected <= self itemsCount])
		ifTrue: [firstVisible := firstVisible + 1].
	self changed