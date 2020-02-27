Class {
	#name : #SpSearchableListTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #deprecation }
SpSearchableListTest class >> isDeprecated [
	^ true
]

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
