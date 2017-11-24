"
A SugsMorphicTextAdapterContext is a context that know how to work with the MorphicTextAdapter.

"
Class {
	#name : #SugsMorphicTextAdapterContext,
	#superclass : #SugsAbstractContext,
	#category : #'SmartSuggestions-Menu'
}

{ #category : #accessing }
SugsMorphicTextAdapterContext >> code [
	^ model getText .
]

{ #category : #refactoring }
SugsMorphicTextAdapterContext >> contentSelection [
	^model widget textArea selection
]

{ #category : #refactoring }
SugsMorphicTextAdapterContext >> selectedClass [
	^model behavior
]

{ #category : #selection }
SugsMorphicTextAdapterContext >> selectedInterval [
	^ model selectionInterval
]

{ #category : #selection }
SugsMorphicTextAdapterContext >> selectedMethod [
	^model model owner selectedMessage
]

{ #category : #refactoring }
SugsMorphicTextAdapterContext >> selectionInterval [
	^self selectedInterval 
]

{ #category : #refactoring }
SugsMorphicTextAdapterContext >> sourceTextArea [
	^model widget  
]
