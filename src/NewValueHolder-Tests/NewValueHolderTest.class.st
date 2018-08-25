"
Unit tests for new ValueHolder
"
Class {
	#name : #NewValueHolderTest,
	#superclass : #TestCase,
	#category : #'NewValueHolder-Tests'
}

{ #category : #tests }
NewValueHolderTest >> testChange [
	| valueHolder fired |
	valueHolder := 'word' asValueHolder.
	self assert: valueHolder value equals: 'word'.
	fired := false.
	valueHolder whenChangedDo: [ fired := true ].
	valueHolder value: 'new'.
	self assert: fired.
	self assert: valueHolder value equals: 'new'
]

{ #category : #tests }
NewValueHolderTest >> testValuesInBlock [
	| valueHolder old new ann |
	valueHolder := 'old' asValueHolder.
	valueHolder
		whenChangedDo: [ :newValue :oldValue :a | 
			new := newValue.
			old := oldValue.
			ann := a ].
	valueHolder value: 'new'.
	self assert: old equals: 'old'.
	self assert: new equals: 'new'.
	self assert: (ann isMemberOf: ValueChanged)
]
