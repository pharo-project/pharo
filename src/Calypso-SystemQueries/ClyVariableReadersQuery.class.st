"
I return only methods which read given variables
"
Class {
	#name : #ClyVariableReadersQuery,
	#superclass : #ClyVariableReferencesQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyVariableReadersQuery >> description [

	^'readers of ', self printVariables 
]

{ #category : #testing }
ClyVariableReadersQuery >> doesMethod: aMethod useVar: aVariable [

	^aVariable isReadIn: aMethod
]
