"
I'm a recorded choice when installing a metacello baseline/configuration.
A choice is created whenever there is a conflict between two project versions (loaded and incomming). Due the nature of Matacello, this choice can be asked several times... A choice keeps track of what user selected to avoid asking him every time.
"
Class {
	#name : #IceMetacelloLoadChoice,
	#superclass : #Object,
	#instVars : [
		'projectName',
		'optionA',
		'optionB',
		'choosen'
	],
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #accessing }
IceMetacelloLoadChoice >> choosen [
	^ choosen
]

{ #category : #accessing }
IceMetacelloLoadChoice >> choosen: anObject [
	choosen := anObject
]

{ #category : #testing }
IceMetacelloLoadChoice >> isChoiceBetween: firstOption and: secondOption [

	^ (self optionA = firstOption)
		and: [ self optionB = secondOption ]
]

{ #category : #accessing }
IceMetacelloLoadChoice >> optionA [
	^ optionA
]

{ #category : #accessing }
IceMetacelloLoadChoice >> optionA: anObject [
	optionA := anObject
]

{ #category : #accessing }
IceMetacelloLoadChoice >> optionB [
	^ optionB
]

{ #category : #accessing }
IceMetacelloLoadChoice >> optionB: anObject [
	optionB := anObject
]

{ #category : #accessing }
IceMetacelloLoadChoice >> projectName [
	^ projectName
]

{ #category : #accessing }
IceMetacelloLoadChoice >> projectName: anObject [
	projectName := anObject
]
