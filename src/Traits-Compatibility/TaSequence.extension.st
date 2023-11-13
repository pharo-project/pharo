Extension { #name : 'TaSequence' }

{ #category : '*Traits-Compatibility' }
TaSequence >> includesElement: anElement [
	^ members anySatisfy: [ :e | e includesElement: anElement ]
]

{ #category : '*Traits-Compatibility' }
TaSequence >> transformations [
	^ members flatCollect: #transformations
]
