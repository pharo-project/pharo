"
A test for supplying answer 
"
Class {
	#name : #SupplyAnswerTest,
	#superclass : #TestCase,
	#category : #'Morphic-Tests-Widgets'
}

{ #category : #tests }
SupplyAnswerTest >> testChooseDirectory [
	| tmpDirectory answer |
	tmpDirectory := FileSystem / 'tmp'.

	answer := [ MorphicUIManager new chooseDirectory ] valueSupplyingAnswer: {'Choose Directory' . tmpDirectory}.

	self assert: answer equals: tmpDirectory
]

{ #category : #tests }
SupplyAnswerTest >> testFillInTheBlank [
	| answer |
	answer := [ MorphicUIManager new request: 'Your favorite color?' ] valueSupplyingAnswer: #('Your favorite color?' 'blue').

	self assert: answer equals: 'blue'
]

{ #category : #tests }
SupplyAnswerTest >> testInform [
	| answer |
	[ MorphicUIManager new inform: 'blah' ]
		on: Exception
		do: [ :ex | answer := ex messageText ].
	self assert: answer equals: 'blah'
]
