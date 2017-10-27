menu - change set
newSet

	| aSet |
	self okToChange ifFalse: [ ^ self ].
	aSet := self model createNewSet.
	aSet ifNotNil:
		[	self updateChangesList.
			changesListPresenter setSelectedItem: aSet ]