"
I am a query of all classes from given scope
"
Class {
	#name : #ClyAllClasses,
	#superclass : #ClyClassQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyAllClasses >> description [

	^'all classes'
]

{ #category : #testing }
ClyAllClasses >> selectsClass: aClass [
	^true
]
