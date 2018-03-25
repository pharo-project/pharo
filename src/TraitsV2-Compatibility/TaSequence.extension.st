Extension { #name : #TaSequence }

{ #category : #'*TraitsV2-Compatibility' }
TaSequence >> includesElement: anElement [ 
	^ members anySatisfy: [ :e | e includesElement: anElement ]
]

{ #category : #'*TraitsV2-Compatibility' }
TaSequence >> transformations [
	^ members flatCollect: #transformations
]
