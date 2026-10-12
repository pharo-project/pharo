Extension { #name : 'TaSequence' }

{ #category : '*Ring-TraitsSupport' }
TaSequence >> includesElement: anElement [

	^ self members anySatisfy: [ :e | e includesElement: anElement ]
]
