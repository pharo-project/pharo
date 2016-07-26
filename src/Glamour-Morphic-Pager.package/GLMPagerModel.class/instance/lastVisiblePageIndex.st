accessing
lastVisiblePageIndex
	| lastIndex |
	
	lastIndex := self firstVisiblePageIndex + self numberOfVisiblePages - 1.
	^ lastIndex > self size
		ifTrue: [ self size ]
		ifFalse: [ lastIndex ]