"
I'm a domain object used by Equals tests.

I'm an Person with a name and an age.
"
Class {
	#name : #EqPerson,
	#superclass : #Object,
	#traits : 'TEquality',
	#classTraits : 'TEquality classTrait',
	#instVars : [
		'name',
		'age'
	],
	#category : #'Equals-Tests'
}

{ #category : #comparing }
EqPerson class >> instVarNamesForEqualityComparison [
	^#(age)
]

{ #category : #'instance creation' }
EqPerson class >> name: aString age: positiveInt [
	^self new
		name: aString;
		age: positiveInt;
		yourself
]

{ #category : #accessing }
EqPerson >> age [
	^ age
]

{ #category : #accessing }
EqPerson >> age: anObject [
	age := anObject
]

{ #category : #accessing }
EqPerson >> name [
	^ name
]

{ #category : #accessing }
EqPerson >> name: anObject [
	name := anObject
]
