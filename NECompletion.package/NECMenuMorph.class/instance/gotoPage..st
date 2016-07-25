paging
gotoPage: anInteger
	| item |
	item := (anInteger - 1) * self pageHeight + 1.
	item >= self itemsCount
		ifTrue: [ ^ self ].
	item := item max: 1.
	firstVisible := item.
	self selected: firstVisible