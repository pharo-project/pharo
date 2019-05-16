Extension { #name : #FTBasicItem }

{ #category : #'*Spec-MorphicAdapters' }
FTBasicItem >> expandedChildren [
	
	self isExpanded ifFalse: [ ^ #() ].
	^ self children , (self children flatCollect: [ :e | e expandedChildren ])
]

{ #category : #'*Spec-MorphicAdapters' }
FTBasicItem >> withExpandedChildren [
	
	^ { self } , self expandedChildren 
]
