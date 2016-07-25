paging
pageCount
	| count |
	self itemsCount == self pageHeight
		ifTrue: [^ 1].
	count := self itemsCount // self pageHeight.
	(self itemsCount \\ self pageHeight) > 0 
		ifTrue:[count := count + 1].
	^count