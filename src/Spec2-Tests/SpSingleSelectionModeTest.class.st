Class {
	#name : #SpSingleSelectionModeTest,
	#superclass : #SpAbstractSelectionModeTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #tests }
SpSingleSelectionModeTest >> testSubscriptionsAreTransfered [
	| count |
	count := 0.
	presenter beSingleSelection.
	presenter whenSelectionChangedDo: [ count := count + 1 ].
	presenter beMultipleSelection.
	presenter selectIndex: 2.
	self assert: count equals: 1
]
