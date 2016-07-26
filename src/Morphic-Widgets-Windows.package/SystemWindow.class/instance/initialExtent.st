open/close
initialExtent
	^ (self model respondsTo: #initialExtent)
		ifTrue: [self model initialExtent]
		ifFalse: [RealEstateAgent standardWindowExtent]