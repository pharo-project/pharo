Class {
	#name : #SpSearchableListTest,
	#superclass : #SpSmokeTestCase,
	#category : #'Spec2-PolyWidgets-Tests'
}

{ #category : #running }
SpSearchableListTest >> classToTest [
	^ SpSearchableList
]

{ #category : #tests }
SpSearchableListTest >> testFiltering [
	presenter items: {'aa' . 'bb' . 'ab' . 'ba'}.
	presenter searchPresenter text: 'a'.
	self assertCollection: presenter listPresenter items hasSameElements: {'aa' . 'ab'}
]
