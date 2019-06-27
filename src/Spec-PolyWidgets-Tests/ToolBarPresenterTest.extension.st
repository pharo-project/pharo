Extension { #name : #ToolBarPresenterTest }

{ #category : #'*Spec-PolyWidgets-Tests' }
ToolBarPresenterTest >> testAddItemPosition [
	| itemLeft itemRight |

	self assert: presenter items isEmpty.
	presenter addItem: (itemLeft := self newToolbarItem) position: ToolBarItemPosition left.
	presenter addItem: (itemRight := self newToolbarItem) position: ToolBarItemPosition right.
	self 
		assert: presenter items 
		equals: (Dictionary newFromPairs: { 
			ToolBarItemPosition left. { itemLeft } asOrderedCollection.
			ToolBarItemPosition right. { itemRight } asOrderedCollection })
]

{ #category : #'*Spec-PolyWidgets-Tests' }
ToolBarPresenterTest >> testItems [ 
	| item |

	self assert: presenter items isEmpty.
	presenter addItem: (item := self newToolbarItem).
	self 
		assert: presenter items 
		equals: (Dictionary newFromPairs: { ToolBarItemPosition left. { item } asOrderedCollection })
]
