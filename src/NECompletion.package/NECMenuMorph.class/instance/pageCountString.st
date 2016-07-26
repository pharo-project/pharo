paging
pageCountString
	^ self itemsCount = 501
		ifTrue: [ 'more' ]
		ifFalse: [ self pageCount asString ]