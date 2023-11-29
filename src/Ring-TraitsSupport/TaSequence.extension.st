Extension { #name : 'TaSequence' }

{ #category : '*Ring-TraitsSupport' }
TaSequence >> includesElement: anElement [
	^ members anySatisfy: [ :e | e includesElement: anElement ]
]
