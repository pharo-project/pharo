accessing
gatherInvisibleItems
			
	^ self band subbricks select: [ :each | (self isPartiallyVisible: each) not ]