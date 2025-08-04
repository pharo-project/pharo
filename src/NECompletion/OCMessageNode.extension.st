Extension { #name : 'OCMessageNode' }

{ #category : '*NECompletion' }
OCMessageNode >> completionEntries: offset [

	| selectors |
	selectors := (receiver hasProperty: #type)
		             ifTrue: [ (receiver propertyAt: #type) allSelectors ]
		             ifFalse: [ Symbol selectorTable ].

	^ selectors asOrderedCollection
		  select: [ :each | each beginsWith: self selector ]
		  thenCollect: [ :each | NECSelectorEntry contents: each node: self ]
]

{ #category : '*NECompletion' }
OCMessageNode >> completionToken: offset [

	^ self selector
]
