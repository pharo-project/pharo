"
I am a context that know how to work with 
a basic  Workspace.
"
Class {
	#name : #SugsWorkspaceContext,
	#superclass : #SugsAbstractContext,
	#category : 'SmartSuggestions-Menu'
}

{ #category : #selection }
SugsWorkspaceContext >> selectedClass [
	^model selectedClassOrMetaClass
]

{ #category : #selection }
SugsWorkspaceContext >> selectedMethod [
	^ model selectedMethod.
]

{ #category : #refactoring }
SugsWorkspaceContext >> sourceTextArea [
	^ model sourceTextArea
]
