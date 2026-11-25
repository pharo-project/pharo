Extension { #name : 'LayoutFrame' }

{ #category : '*Morphic-Widgets' }
LayoutFrame >> isHorizontallyResizeable [

	^ self rightFraction ~= self leftFraction
]

{ #category : '*Morphic-Widgets' }
LayoutFrame >> isVerticallyResizeable [

	^ self bottomFraction ~= self topFraction
]
