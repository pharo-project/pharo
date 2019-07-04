"
I return all methods which are visible from the scope.
"
Class {
	#name : #ClyAllMethodsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyAllMethodsQuery >> description [

	^'all methods'
]

{ #category : #testing }
ClyAllMethodsQuery >> selectsMethod: aMethod [
	^true
]
