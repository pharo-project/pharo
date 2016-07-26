actions
showDetail
	detailMorph ifNotNil: [ ^ self browse ].
	self itemsCount isZero ifTrue: [ ^ self ].
	detailMorph := NECDetailMorph new.
	self addMorph: detailMorph.
	self updateDetail