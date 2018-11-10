Class {
	#name : #IceTipFilter,
	#superclass : #Object,
	#instVars : [
		'dataSource',
		'pattern'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #'instance creation' }
IceTipFilter class >> pattern: aString [ 
	^ self new pattern: aString
]

{ #category : #accessing }
IceTipFilter >> dataSource [
	^ dataSource
]

{ #category : #accessing }
IceTipFilter >> dataSource: anObject [
	dataSource := anObject
]

{ #category : #execution }
IceTipFilter >> execute [
	| newElements |
	
	newElements := self dataSource elements species new.
	self dataSource elements do: [ :each |
		(self matches: each) 
			ifTrue: [ newElements add: each ] ].

	^ self dataSource copy
		elements: newElements;
		yourself
]

{ #category : #execution }
IceTipFilter >> executeOn: aDataSource [
	self dataSource: aDataSource.
	^ self execute
]

{ #category : #testing }
IceTipFilter >> matches: anObject [
	self subclassResponsibility
]

{ #category : #accessing }
IceTipFilter >> pattern [
	^ pattern
]

{ #category : #accessing }
IceTipFilter >> pattern: anObject [
	pattern := anObject
]
