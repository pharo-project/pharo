"
Data object for a grid example
"
Class {
	#name : #PersonDataExample,
	#superclass : #Object,
	#instVars : [
		'firstName',
		'secondName',
		'age',
		'married'
	],
	#category : #Morphic-Examples-MorphTreeWidget
}

{ #category : #'instance creation' }
PersonDataExample class >> firstName: fname secondName: sname age: age married: married [
	^ (self new) firstName: fname; secondName: sname; age: age; married: married; yourself
]

{ #category : #accessing }
PersonDataExample >> age [
	"Answer the value of age"

	^ age
]

{ #category : #accessing }
PersonDataExample >> age: anObject [
	"Set the value of age"

	age := anObject
]

{ #category : #accessing }
PersonDataExample >> firstName [
	"Answer the value of firstName"

	^ firstName
]

{ #category : #accessing }
PersonDataExample >> firstName: anObject [
	"Set the value of firstName"

	firstName := anObject
]

{ #category : #accessing }
PersonDataExample >> married [
	"Answer the value of married"

	^ married
]

{ #category : #accessing }
PersonDataExample >> married: anObject [
	"Set the value of married"

	married := anObject
]

{ #category : #accessing }
PersonDataExample >> secondName [
	"Answer the value of secondName"

	^ secondName
]

{ #category : #accessing }
PersonDataExample >> secondName: anObject [
	"Set the value of secondName"

	secondName := anObject
]
