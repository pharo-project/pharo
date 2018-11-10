"
I return all methods which are visible from the scope.
"
Class {
	#name : #ClyAllMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyAllMethods >> description [

	^'all methods'
]

{ #category : #testing }
ClyAllMethods >> selectsMethod: aMethod [
	^true
]
