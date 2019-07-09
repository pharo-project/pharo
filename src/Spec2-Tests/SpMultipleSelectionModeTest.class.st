Class {
	#name : #SpMultipleSelectionModeTest,
	#superclass : #SpAbstractSelectionModeTest,
	#category : #'Spec2-Tests-Layout'
}

{ #category : #tests }
SpMultipleSelectionModeTest >> testSubscriptionsAreTransfered [
	| count |
	count := 0.
	presenter beMultipleSelection.
	presenter whenSelectionChangedDo: [ count := count + 1 ].
	presenter beSingleSelection.
	presenter selectIndex: 2.
	self assert: count equals: 1
]
