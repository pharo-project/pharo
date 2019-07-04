"
I am a query of all classes from given scope
"
Class {
	#name : #ClyAllClassesQuery,
	#superclass : #ClyClassQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyAllClassesQuery >> description [

	^'all classes'
]

{ #category : #testing }
ClyAllClassesQuery >> selectsClass: aClass [
	^true
]
