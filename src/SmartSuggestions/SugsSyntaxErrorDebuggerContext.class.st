"
I know how to talk with a syntax error debugger and extract the information needed by the suggestions
"
Class {
	#name : #SugsSyntaxErrorDebuggerContext,
	#superclass : #SugsAbstractContext,
	#category : #SmartSuggestions-Menu
}

{ #category : #refactoring }
SugsSyntaxErrorDebuggerContext >> selectedClass [
	^model selectedClass
]

{ #category : #refactoring }
SugsSyntaxErrorDebuggerContext >> selectedMethod [

	^ nil
	
]

{ #category : #refactoring }
SugsSyntaxErrorDebuggerContext >> sourceTextArea [
	^ model dependents first findDeepSubmorphThat: [ :m | m isKindOf: RubScrolledTextMorph ] ifAbsent: [ self error: 'should not happen' ]
]
