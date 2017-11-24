Class {
	#name : #MetacelloMethodSpec,
	#superclass : #Object,
	#instVars : [
		'project',
		'selector',
		'category',
		'versionString',
		'methodSections'
	],
	#category : #'Metacello-Core-Constructors'
}

{ #category : #accessing }
MetacelloMethodSpec >> category [
	^ category
]

{ #category : #accessing }
MetacelloMethodSpec >> category: anObject [
	category := anObject
]

{ #category : #'method generation' }
MetacelloMethodSpec >> compileMethod [

	(project configuration class
		compile: self methodSource
		classified: self category) == nil
		ifTrue: [ self error: 'Error compiling the method' ]
]

{ #category : #accessing }
MetacelloMethodSpec >> methodSections [

	methodSections == nil ifTrue: [ methodSections := OrderedCollection new ].
	^ methodSections
]

{ #category : #accessing }
MetacelloMethodSpec >> methodSections: anObject [
	methodSections := anObject
]

{ #category : #'method generation' }
MetacelloMethodSpec >> methodSource [

	self subclassResponsibility
]

{ #category : #accessing }
MetacelloMethodSpec >> project [
	^ project
]

{ #category : #accessing }
MetacelloMethodSpec >> project: anObject [
	project := anObject
]

{ #category : #accessing }
MetacelloMethodSpec >> selector [
	^ selector
]

{ #category : #accessing }
MetacelloMethodSpec >> selector: anObject [
	selector := anObject
]

{ #category : #accessing }
MetacelloMethodSpec >> versionString [
	^ versionString
]

{ #category : #accessing }
MetacelloMethodSpec >> versionString: aStringOrSymbol [
	versionString := aStringOrSymbol
]
