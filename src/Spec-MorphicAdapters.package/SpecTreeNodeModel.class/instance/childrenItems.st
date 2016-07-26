accessing
childrenItems
	^ self model autoRefreshOnExpand
		ifTrue: [ childrenItemsBlock value ]
		ifFalse: [ childrenItems ifNil: [ childrenItems := childrenItemsBlock value ] ]