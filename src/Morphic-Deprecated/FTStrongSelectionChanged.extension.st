Extension { #name : 'FTStrongSelectionChanged' }

{ #category : '*Morphic-Deprecated' }
FTStrongSelectionChanged >> selectedRowIndex [
	self deprecated: 'Use #selectedIndex instead' transformWith: '`@receiver selectedRowIndex' -> '`@receiver selectedIndex'.
	^ self selectedIndex
]
