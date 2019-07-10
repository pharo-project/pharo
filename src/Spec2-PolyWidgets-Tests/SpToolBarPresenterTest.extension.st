Extension { #name : #SpToolBarPresenterTest }

{ #category : #'*Spec2-PolyWidgets-Tests' }
SpToolBarPresenterTest >> testAddItemPosition [
	| itemLeft itemRight |

	self assert: presenter items isEmpty.
	presenter addItem: (itemLeft := self newToolbarItem) position: SpToolBarItemPosition left.
	presenter addItem: (itemRight := self newToolbarItem) position: SpToolBarItemPosition right.
	self 
		assert: presenter items 
		equals: (Dictionary newFromPairs: { 
			SpToolBarItemPosition left. { itemLeft } asOrderedCollection.
			SpToolBarItemPosition right. { itemRight } asOrderedCollection })
]

{ #category : #'*Spec2-PolyWidgets-Tests' }
SpToolBarPresenterTest >> testItems [ 
	| item |

	self assert: presenter items isEmpty.
	presenter addItem: (item := self newToolbarItem).
	self 
		assert: presenter items 
		equals: (Dictionary newFromPairs: { SpToolBarItemPosition left. { item } asOrderedCollection })
]
