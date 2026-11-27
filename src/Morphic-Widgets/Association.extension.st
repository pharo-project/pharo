Extension { #name : 'Association' }

{ #category : '*Morphic-Widgets' }
Association >> treeNodeHead [

	^ (key isKindOf: Association)
		ifTrue: [ key treeNodeHead ]
		ifFalse: [ key ]
]

{ #category : '*Morphic-Widgets' }
Association >> treeNodeTail [

	^ (key isKindOf: Association)
		ifTrue: [ key treeNodeTail -> value ]
		ifFalse: [ value ]
]
