"
I'm a domain object used by Equals tests.

I'm a Teacher with an experience level.
"
Class {
	#name : #EqTeacher,
	#superclass : #EqPerson,
	#instVars : [
		'experience'
	],
	#category : #'Equals-Tests'
}

{ #category : #comparing }
EqTeacher class >> instVarNamesForEqualityComparison [
	^super instVarNamesForEqualityComparison copyWith: #experience
]

{ #category : #accessing }
EqTeacher >> experience [
	^ experience
]

{ #category : #accessing }
EqTeacher >> experience: anObject [
	experience := anObject
]

{ #category : #initialization }
EqTeacher >> initialize [
	super initialize.
	self experience: 0
]
