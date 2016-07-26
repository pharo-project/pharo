actions
remove
	self canRemove
		ifFalse: [ ^ self ].
	self list removeAt: self index.
	self changedList.
	self updateIndex