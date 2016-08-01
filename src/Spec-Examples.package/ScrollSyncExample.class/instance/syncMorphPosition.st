private
syncMorphPosition
	currentVerticalScrollPosition = text scrollValue y
		ifTrue: [ ^ self ].
	currentVerticalScrollPosition := text scrollValue y.
	self needRebuild: false.
	text needRebuild: false.
	self buildWithSpecLayout: self defaultSpecLayout

	