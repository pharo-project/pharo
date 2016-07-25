t - accessing
selection

	| list |
	list := self rootItems.
	(list notEmpty and: [ super selection isNil ])
		ifTrue: [	self hardlySelectItem: list first ].
	^ super selection