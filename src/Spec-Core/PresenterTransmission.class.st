"
A presenter transmission modelates a transmission from one output port to an input port.
A transmission is a way to handle how a presenter will handle input/output by default.

It uses ""ports"" to define how data passes, but it can also transform it (see #transform:) to make it pass in a correct way.
"
Class {
	#name : #PresenterTransmission,
	#superclass : #Object,
	#instVars : [
		'presenter',
		'fromPort',
		'toPort',
		'transformBlock',
		'preTransmission',
		'postTransmission'
	],
	#category : #'Spec-Core-Transmission'
}

{ #category : #'instance creation' }
PresenterTransmission class >> on: aPresenter [

	^ self basicNew 
		initializePresenter: aPresenter;
		yourself
]

{ #category : #private }
PresenterTransmission >> apply [
	fromPort attachTransmission: self
]

{ #category : #private }
PresenterTransmission >> applyPostTransmissionWith: aTransmittedObject [

	self postTransmission ifNil: [ ^ self ].
	self postTransmission 
		cull: self toPresenter 
		cull: self fromPresenter 
		cull: aTransmittedObject
]

{ #category : #private }
PresenterTransmission >> applyPreTransmissionWith: aTransmittedObject [

	self preTransmission ifNil: [ ^ self ].
	self preTransmission 
		cull: self toPresenter 
		cull: self fromPresenter 
		cull: aTransmittedObject
]

{ #category : #private }
PresenterTransmission >> from: aPresenter [
	self fromPort: aPresenter outputPortDefault
]

{ #category : #script }
PresenterTransmission >> from: aPresenter port: aSymbol [
	self fromPort: (aPresenter outputPortNamed: aSymbol)
]

{ #category : #actions }
PresenterTransmission >> from: aPresenter port: aSymbol to: anotherPresenter transform: aValuable [
	self
		from: aPresenter
		port: aSymbol
		to: anotherPresenter
		transform: aValuable
		postTransmission: nil
]

{ #category : #actions }
PresenterTransmission >> from: aPresenter port: aSymbol to: anotherPresenter transform: aValuable postTransmission: anotherValuable [

	self
		from: aPresenter port: aSymbol;
		to: anotherPresenter;
		transform: aValuable;
		postTransmission: anotherValuable;
		apply
]

{ #category : #actions }
PresenterTransmission >> from: aPresenter to: anotherPresenter [
	
	self
		from: aPresenter
		to: anotherPresenter
		transform: nil
]

{ #category : #actions }
PresenterTransmission >> from: aPresenter to: anotherPresenter transform: aValuable [
	
	self
		from: aPresenter
		to: anotherPresenter
		transform: aValuable
		postTransmission: nil
]

{ #category : #actions }
PresenterTransmission >> from: aPresenter to: anotherPresenter transform: aValuable postTransmission: anotherValuable [
	self
		from: aPresenter;
		to: anotherPresenter;
		transform: aValuable;
		postTransmission: anotherValuable;
		apply
]

{ #category : #accessing }
PresenterTransmission >> fromPort [

	^ fromPort
]

{ #category : #private }
PresenterTransmission >> fromPort: aPort [
	fromPort := aPort
]

{ #category : #private }
PresenterTransmission >> fromPresenter [
	^ self fromPort presenter
]

{ #category : #initialization }
PresenterTransmission >> initializePresenter: aPresenter [

	self initialize.
	presenter := aPresenter
]

{ #category : #accessing }
PresenterTransmission >> postTransmission [
	^ postTransmission
]

{ #category : #accessing }
PresenterTransmission >> postTransmission: aValuable [
	postTransmission := aValuable
]

{ #category : #accessing }
PresenterTransmission >> preTransmission [
	^ preTransmission
]

{ #category : #accessing }
PresenterTransmission >> preTransmission: aValuable [
	preTransmission := aValuable
]

{ #category : #private }
PresenterTransmission >> presenter [

	^ presenter
]

{ #category : #private }
PresenterTransmission >> to: aPresenter [

	self toPort: aPresenter inputPortDefault
]

{ #category : #script }
PresenterTransmission >> to: aPresenter port: aSymbol [

	self toPort: (aPresenter inputPortNamed: aSymbol)
]

{ #category : #accessing }
PresenterTransmission >> toPort [

	^ toPort
]

{ #category : #private }
PresenterTransmission >> toPort: aPort [
	toPort := aPort
]

{ #category : #private }
PresenterTransmission >> toPresenter [
	^ self toPort presenter
]

{ #category : #script }
PresenterTransmission >> transform: aBlock [

	transformBlock := aBlock
]

{ #category : #private }
PresenterTransmission >> transformed: anObject [ 

	transformBlock ifNil: [ ^ anObject ].
	^ transformBlock value: anObject
]
