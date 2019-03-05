Class {
	#name : #SearchableListTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-PolyWidgets-ListAndTree'
}

{ #category : #running }
SearchableListTest >> classToTest [
	^ SearchableList
]

{ #category : #tests }
SearchableListTest >> testFiltering [
	presenter listPresenter items: {'aa' . 'bb' . 'ab' . 'ba'}.
	presenter searchPresenter text: 'a'.
	self assert: presenter listPresenter model shownItems asArray equals: {'aa' . 'ab'}
]
