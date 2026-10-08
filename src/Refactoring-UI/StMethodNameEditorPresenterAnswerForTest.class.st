Class {
	#name : 'StMethodNameEditorPresenterAnswerForTest',
	#superclass : 'Object',
	#instVars : [
		'label',
		'answers'
	],
	#category : 'Refactoring-UI-UI',
	#package : 'Refactoring-UI',
	#tag : 'UI'
}

{ #category : 'accessing' }
StMethodNameEditorPresenterAnswerForTest >> answers: aCollection [
	"Queue the answers to be returned, in order, one per request."

	answers := aCollection asOrderedCollection
]

{ #category : 'accessing' }
StMethodNameEditorPresenterAnswerForTest >> label [
	"Answer the next queued answer, if any, or the fixed label."

	answers isNil ifTrue: [ ^ label ].
	answers isEmpty ifTrue: [ ^ label ].
	^ answers removeFirst
]

{ #category : 'accessing' }
StMethodNameEditorPresenterAnswerForTest >> label: aString [

	label := aString
]
