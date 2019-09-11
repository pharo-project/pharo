"
I return all classes without any tag
"
Class {
	#name : #ClyUntaggedClassesQuery,
	#superclass : #ClyClassQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyUntaggedClassesQuery >> description [

	^'classes without tags'
]

{ #category : #testing }
ClyUntaggedClassesQuery >> selectsClass: aClass [
	^aClass tags isEmpty
]
