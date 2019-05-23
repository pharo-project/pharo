"
A presenter transmission modelates a transmission from one output port to an input port.
A transmission is a way to handle how a presenter will handle input/output by default.

It uses ""ports"" to define how data passes, but it can also transform it (see #transform:) to make it pass in a correct way.
"
Class {
	#name : #SpecTransmission,
	#superclass : #Object,
	#instVars : [
		'fromPort',
		'toPort',
		'transformBlock',
		'preTransmission',
		'postTransmission'
	],
	#category : #'Spec-Transmission-Base'
}

{ #category : #private }
SpecTransmission >> apply [
	fromPort attachTransmission: self
]

{ #category : #private }
SpecTransmission >> applyPostTransmissionWith: aTransmittedObject [

	self postTransmission ifNil: [ ^ self ].
	self postTransmission 
		cull: self toPresenter 
		cull: self fromPresenter 
		cull: aTransmittedObject
]

{ #category : #private }
SpecTransmission >> applyPreTransmissionWith: aTransmittedObject [

	self preTransmission ifNil: [ ^ self ].
	self preTransmission 
		cull: self toPresenter 
		cull: self fromPresenter 
		cull: aTransmittedObject
]

{ #category : #actions }
SpecTransmission >> do: aBlock [
	"Will create a generic action block to handle transmission"
	
	self toPort: (SpecActionPort newAction: aBlock)
]

{ #category : #private }
SpecTransmission >> from: aPresenter [
	self fromPort: aPresenter outputPortDefault
]

{ #category : #script }
SpecTransmission >> from: aPresenter port: aSymbol [
	self fromPort: (aPresenter outputPortNamed: aSymbol)
]

{ #category : #actions }
SpecTransmission >> from: aPresenter port: aSymbol to: anotherPresenter transform: aValuable [
	self
		from: aPresenter
		port: aSymbol
		to: anotherPresenter
		transform: aValuable
		postTransmission: nil
]

{ #category : #actions }
SpecTransmission >> from: aPresenter port: aSymbol to: anotherPresenter transform: aValuable postTransmission: anotherValuable [

	self
		from: aPresenter port: aSymbol;
		to: anotherPresenter;
		transform: aValuable;
		postTransmission: anotherValuable;
		apply
]

{ #category : #actions }
SpecTransmission >> from: aPresenter to: anotherPresenter [
	
	self
		from: aPresenter
		to: anotherPresenter
		transform: nil
]

{ #category : #actions }
SpecTransmission >> from: aPresenter to: anotherPresenter transform: aValuable [
	
	self
		from: aPresenter
		to: anotherPresenter
		transform: aValuable
		postTransmission: nil
]

{ #category : #actions }
SpecTransmission >> from: aPresenter to: anotherPresenter transform: aValuable postTransmission: anotherValuable [
	self
		from: aPresenter;
		to: anotherPresenter;
		transform: aValuable;
		postTransmission: anotherValuable;
		apply
]

{ #category : #accessing }
SpecTransmission >> fromPort [

	^ fromPort
]

{ #category : #private }
SpecTransmission >> fromPort: aPort [
	fromPort := aPort
]

{ #category : #private }
SpecTransmission >> fromPresenter [
	^ self fromPort presenter
]

{ #category : #accessing }
SpecTransmission >> postTransmission [
	^ postTransmission
]

{ #category : #accessing }
SpecTransmission >> postTransmission: aValuable [
	postTransmission := aValuable
]

{ #category : #accessing }
SpecTransmission >> preTransmission [
	^ preTransmission
]

{ #category : #accessing }
SpecTransmission >> preTransmission: aValuable [
	preTransmission := aValuable
]

{ #category : #private }
SpecTransmission >> to: aPresenter [

	self toPort: aPresenter inputPortDefault
]

{ #category : #script }
SpecTransmission >> to: aPresenter port: aSymbol [

	self toPort: (aPresenter inputPortNamed: aSymbol)
]

{ #category : #accessing }
SpecTransmission >> toPort [

	^ toPort
]

{ #category : #private }
SpecTransmission >> toPort: aPort [
	toPort := aPort
]

{ #category : #private }
SpecTransmission >> toPresenter [
	^ self toPort presenter
]

{ #category : #script }
SpecTransmission >> transform: aBlock [

	transformBlock := aBlock
]

{ #category : #private }
SpecTransmission >> transformed: anObject [ 

	transformBlock ifNil: [ ^ anObject ].
	^ transformBlock value: anObject
]
