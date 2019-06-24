Extension { #name : #LayoutFrame }

{ #category : #'*Morphic-Widgets-Windows' }
LayoutFrame >> isHorizontallyResizeable [

	^ self rightFraction ~= self leftFraction
]

{ #category : #'*Morphic-Widgets-Windows' }
LayoutFrame >> isVerticallyResizeable [

	^ self bottomFraction ~= self topFraction
]
