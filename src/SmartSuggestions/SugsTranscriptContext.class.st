"
I know how to use a Transcript to extract the information needed for the suggestions
"
Class {
	#name : #SugsTranscriptContext,
	#superclass : #SugsAbstractContext,
	#category : 'SmartSuggestions-Menu'
}

{ #category : #refactoring }
SugsTranscriptContext >> selectedClass [
	^nil
]

{ #category : #refactoring }
SugsTranscriptContext >> selectedMethod [

	^nil
]

{ #category : #refactoring }
SugsTranscriptContext >> sourceTextArea [
	| window |
	window := model dependents ifEmpty: [ nil ] ifNotEmpty: [ model dependents at: 1  ].
	
	^window submorphThat: [ :each | each respondsTo: #selectionInterval ] ifNone: [ nil ]
]
