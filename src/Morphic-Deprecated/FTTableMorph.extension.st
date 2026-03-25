Extension { #name : 'FTTableMorph' }

{ #category : '*Morphic-Deprecated' }
FTTableMorph >> isRowIndexVisible: rowIndex [
	self deprecated: 'Use #isIndexVisible: instead' transformWith: '`@receiver isRowIndexVisible: `@statements1' -> '`@receiver isIndexVisible: `@statements1'.
	^ self isIndexVisible: rowIndex
]

{ #category : '*Morphic-Deprecated' }
FTTableMorph >> selectRowIndex: aNumber [
	self deprecated: 'Use #selectIndex: instead' transformWith: '`@receiver selectRowIndex: `@statements1' -> '`@receiver selectIndex: `@statements1'.
	self selectIndex: aNumber
]
