lazy scrolling
updateVisibleBottom: invisibleBricks
	|after|
	
	after := self band lastSubbrick.
	[ after isNotNil and: [ (self isPartiallyVisible: after) and: [ (self getIndex: after) < self model count ] ] ] whileTrue: [
		after := self
			addItemMorph: (
				self
					getMorphAt: (self getIndex: after) + 1
					recycled: (self getAndRemoveInvisibleMorph: invisibleBricks))
			after: after ].