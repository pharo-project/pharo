actions
down
	self canMoveDown
		ifFalse: [ ^ self ].
	self list swap: self index with: self index + 1.
	self index: self index + 1.
	self changedList