Extension { #name : #GLMMorphicRenderer }

{ #category : #'*Glamour-Morphic-Pager' }
GLMMorphicRenderer >> renderPager: aBrowser [ 
	^ aBrowser hasFixedSizePanes 
		ifTrue: [GLMMorphicPagerRenderer render: aBrowser from: self]
		ifFalse: []
]
