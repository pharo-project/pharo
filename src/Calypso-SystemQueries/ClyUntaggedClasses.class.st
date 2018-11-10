"
I return all classes without any tag
"
Class {
	#name : #ClyUntaggedClasses,
	#superclass : #ClyClassQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyUntaggedClasses >> description [

	^'classes without tags'
]

{ #category : #testing }
ClyUntaggedClasses >> selectsClass: aClass [
	^aClass tags isEmpty
]
