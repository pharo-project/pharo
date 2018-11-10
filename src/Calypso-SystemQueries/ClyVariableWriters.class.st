"
I return only methods which write given variables
"
Class {
	#name : #ClyVariableWriters,
	#superclass : #ClyVariableReferences,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyVariableWriters >> description [

	^'writers of ', self printVariables 
]

{ #category : #testing }
ClyVariableWriters >> doesMethod: aMethod useVar: aVariable [

	^aVariable isWrittenIn: aMethod
]
