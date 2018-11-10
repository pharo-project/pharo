"
I return all methods which use undeclared variables
"
Class {
	#name : #ClyUndeclaredMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-Undeclared-Queries'
}

{ #category : #printing }
ClyUndeclaredMethods >> description [

	^'methods with undeclared variables'
]

{ #category : #testing }
ClyUndeclaredMethods >> selectsMethod: aMethod [
	^aMethod usesUndeclares
]
