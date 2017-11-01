Extension { #name : #Association }

{ #category : #'*Morphic-Base' }
Association >> head [

	^ (key isKindOf: Association)
		ifTrue: [ key head ]
		ifFalse: [ key ]
]

{ #category : #'*Morphic-Base' }
Association >> tail [

	^ (key isKindOf: Association)
		ifTrue: [ key tail -> value ]
		ifFalse: [ value ]
]
