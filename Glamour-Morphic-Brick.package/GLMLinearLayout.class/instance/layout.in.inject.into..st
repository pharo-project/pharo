layout
layout: aBrick in: aRectangle inject: anInteger into: aBlock

	aBrick subbricks inject: anInteger into:[ :origin :each | |result|
		result := origin + ((self isFloating: each)
			ifFalse: [ aBlock cull: origin cull: each ]
			ifTrue: [ self layoutFloating: each in: aRectangle ]).
		"applying aligning: #none, #left, #center, #right"
		self align: each in: aBrick anchorPoint: origin inContext: nil.
		each onLayouted.
		result ].