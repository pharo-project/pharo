Extension { #name : 'FTStrongSelectionChanged' }

{ #category : '*Morphic-Deprecated' }
FTStrongSelectionChanged >> selectedRowIndex [
	self
		deprecated: 'Use #selectedIndex instead'
		on: '2025-10-29'
		in: #Pharo14
		transformWith: '`@receiver selectedRowIndex' -> '`@receiver selectedIndex'.
	^ self selectedIndex
]
