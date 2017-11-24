"
I know how to  extract the information needed for the suggestions from a finder UI object
"
Class {
	#name : #SugsFinderContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #refactoring }
SugsFinderContext >> selectedClass [
	
	^ model selectedClass
]

{ #category : #refactoring }
SugsFinderContext >> selectedMethod [
	
	model selectedClass ifNil: [ ^ nil ].
	model selectedMethod ifNil: [ ^ nil ].
	^ model selectedClass >> model selectedMethod
]

{ #category : #refactoring }
SugsFinderContext >> sourceTextArea [
	
	^ model sourceTextArea
]
