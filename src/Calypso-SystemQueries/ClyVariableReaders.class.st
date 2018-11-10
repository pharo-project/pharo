"
I return only methods which read given variables
"
Class {
	#name : #ClyVariableReaders,
	#superclass : #ClyVariableReferences,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyVariableReaders >> description [

	^'readers of ', self printVariables 
]

{ #category : #testing }
ClyVariableReaders >> doesMethod: aMethod useVar: aVariable [

	^aVariable isReadIn: aMethod
]
