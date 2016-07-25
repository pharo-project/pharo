accessing
wantsToBeCachedByHand
	"Return true if the receiver wants to be cached by the hand when it is dragged around.
	Note: The default implementation queries all submorphs since subclasses may have shapes that do not fill the receiver's bounds completely."

	self isTranslucentButNotTransparent ifTrue: [ ^ false ].
	self
		submorphsDo: [ :m | 
			m wantsToBeCachedByHand
				ifFalse: [ ^ false ] ].
	^ true