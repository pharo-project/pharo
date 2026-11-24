Extension { #name : 'FTTableMorph' }

{ #category : '*Morphic-Deprecated' }
FTTableMorph >> isRowIndexVisible: rowIndex [
	self
		deprecated: 'Use #isIndexVisible: instead'
		on: '2025-10-29'
		in: #Pharo14
		transformWith: '`@receiver isRowIndexVisible: `@statements1' -> '`@receiver isIndexVisible: `@statements1'.
	^ self isIndexVisible: rowIndex
]

{ #category : '*Morphic-Deprecated' }
FTTableMorph >> selectRowIndex: aNumber [
	self
		deprecated: 'Use #selectIndex: instead'
		on: '2025-10-29'
		in: #Pharo14
		transformWith: '`@receiver selectRowIndex: `@statements1' -> '`@receiver selectIndex: `@statements1'.
	self selectIndex: aNumber
]
