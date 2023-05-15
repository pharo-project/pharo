"
I'm a domain object used by Equals tests.

I'm an Student with a level of study.
"
Class {
	#name : #EqStudent,
	#superclass : #EqPerson,
	#instVars : [
		'level'
	],
	#category : #'Equals-Tests'
}

{ #category : #comparing }
EqStudent class >> instVarNamesForEqualityComparison [
	^super instVarNamesForEqualityComparison copyWith: #level
]

{ #category : #initialization }
EqStudent >> initialize [
	super initialize.
	self level: 1
]

{ #category : #accessing }
EqStudent >> level [
	^ level
]

{ #category : #accessing }
EqStudent >> level: anObject [
	level := anObject
]
