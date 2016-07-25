lazy scrolling
updateVisibleTop: invisibleBricks
	|before|
	
	before := self band hasSubbricks not
		ifFalse: [ self band firstSubbrick ]
		ifTrue: [ self addBrickBackNoLayout: (self getMorphAt: self visibleIndex recycled: (self getAndRemoveInvisibleMorph: invisibleBricks)) ].
		
	[ before isNotNil and: [ (self isPartiallyVisible: before) and: [ (self getIndex: before) > 1 ] ] ] whileTrue: [
		before := self
			addItemMorph: (
				self
					getMorphAt: (self getIndex: before) - 1
					recycled: (self getAndRemoveInvisibleMorph: invisibleBricks))
			before: before  ]
	