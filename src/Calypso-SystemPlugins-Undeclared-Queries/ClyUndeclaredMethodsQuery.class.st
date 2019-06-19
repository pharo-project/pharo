"
I return all methods which use undeclared variables
"
Class {
	#name : #ClyUndeclaredMethodsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries'
}

{ #category : #printing }
ClyUndeclaredMethodsQuery >> description [

	^'methods with undeclared variables'
]

{ #category : #testing }
ClyUndeclaredMethodsQuery >> selectsMethod: aMethod [
	^aMethod usesUndeclares
]
