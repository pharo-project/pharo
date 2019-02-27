Class {
	#name : #EditableListTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-PolyWidgets-ListAndTree'
}

{ #category : #running }
EditableListTest >> classToTest [
	^ EditableList
]

{ #category : #tests }
EditableListTest >> testMoveElementAtTo [
	presenter list: {'AAA' . 'BBB' . 'CCC'}.
	presenter moveElementAt: 1 to: 3.
	self
		assert: presenter list model items asArray 
		equals: {'BBB' . 'CCC' . 'AAA'}
]
