"
I return only methods which write given variables
"
Class {
	#name : #ClyVariableWritersQuery,
	#superclass : #ClyVariableReferencesQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyVariableWritersQuery >> description [

	^'writers of ', self printVariables 
]

{ #category : #testing }
ClyVariableWritersQuery >> doesMethod: aMethod useVar: aVariable [

	^aVariable isWrittenIn: aMethod
]
