"
I define a default null suggestion context that provides no suggestions
"
Class {
	#name : #SugsNullContext,
	#superclass : #SugsAbstractContext,
	#category : #SmartSuggestions-Menu
}

{ #category : #selection }
SugsNullContext >> selectedClass [
	^ nil
]

{ #category : #selection }
SugsNullContext >> selectedMethod [
	
	^ nil
]

{ #category : #refactoring }
SugsNullContext >> sourceTextArea [
	^ nil
]
