"
I know how to  extract the information needed for the suggestions from a DiffModel
"
Class {
	#name : #SugsDiffModelContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #selection }
SugsDiffModelContext >> selectedClass [
	
	^ model selectedClass
]

{ #category : #selection }
SugsDiffModelContext >> selectedMethod [
	
	^ nil
]

{ #category : #accessing }
SugsDiffModelContext >> sourceTextArea [
	^model sourceTextArea
]
