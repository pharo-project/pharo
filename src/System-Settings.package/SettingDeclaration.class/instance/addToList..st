user interface
addToList: anItem
	(self list includes: anItem)
		ifFalse: [self list size > self maxKept
				ifTrue: [self list removeFirst].
			self list add: anItem.
			self changed: #list]