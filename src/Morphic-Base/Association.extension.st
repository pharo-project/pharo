Extension { #name : #Association }

{ #category : #'*Morphic-Base' }
Association >> treeNodeHead [

	^ (key isKindOf: Association)
		ifTrue: [ key treeNodeHead ]
		ifFalse: [ key ]
]

{ #category : #'*Morphic-Base' }
Association >> treeNodeTail [

	^ (key isKindOf: Association)
		ifTrue: [ key treeNodeTail -> value ]
		ifFalse: [ value ]
]
