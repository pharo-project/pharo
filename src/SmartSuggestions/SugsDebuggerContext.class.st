"
I know how to talk with a debugger and extract the information needed for the suggestions :)
"
Class {
	#name : #SugsDebuggerContext,
	#superclass : #SugsAbstractContext,
	#category : 'SmartSuggestions-Menu'
}

{ #category : #refactoring }
SugsDebuggerContext >> selectedClass [
	^model selectedClass
]

{ #category : #refactoring }
SugsDebuggerContext >> selectedMethod [

	^model selectedContext method
	
]

{ #category : #refactoring }
SugsDebuggerContext >> sourceTextArea [
	^ model dependents at: 3 
]
