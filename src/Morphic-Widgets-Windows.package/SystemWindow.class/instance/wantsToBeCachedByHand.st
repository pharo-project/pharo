testing
wantsToBeCachedByHand
	"Return true if the receiver wants to be cached by the hand when it is dragged around."

	self isTranslucentButNotTransparent
		ifTrue: [ ^ false ].
	self clipSubmorphs
		ifTrue: [ ^ true ].
	self bounds = self fullBounds
		ifTrue: [ ^ true ].
	self
		submorphsDo: [ :m | 
			(self bounds containsRect: m fullBounds)
				ifFalse: [ 
					m wantsToBeCachedByHand
						ifFalse: [ ^ false ] ] ].
	^ true