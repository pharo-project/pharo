scrollbar managing
handleScrollerOffsetChange: aBlock
	| prevOffset ret |
	prevOffset := self offset.
	ret := aBlock value.
	(self offset ~= prevOffset and: [ self owner notNil ])
		ifTrue: [ self owner scrollerOffsetChanged ].
	^ ret