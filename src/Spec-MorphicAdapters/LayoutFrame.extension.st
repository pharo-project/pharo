Extension { #name : #LayoutFrame }

{ #category : #'*Spec-MorphicAdapters' }
LayoutFrame >> isHorizontallyResizeable [

	^ self rightFraction ~= self leftFraction
]

{ #category : #'*Spec-MorphicAdapters' }
LayoutFrame >> isVerticallyResizeable [

	^ self bottomFraction ~= self topFraction
]
