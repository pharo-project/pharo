"
A presenter transmission modelates a transmission from one output port to an input port.
A transmission is a way to handle how a presenter will handle input/output by default.

It uses ""ports"" to define how data passes, but it can also transform it (see #transform:) to make it pass in a correct way.
"
Class {
	#name : #SpTransmission,
	#superclass : #Object,
	#instVars : [
		'fromPort',
		'toPort',
		'transformBlock',
		'preTransmission',
		'postTransmission'
	],
	#category : #'Spec2-Transmission-Base'
}

{ #category : #private }
SpTransmission >> apply [
	fromPort attachTransmission: self
]

{ #category : #private }
SpTransmission >> applyPostTransmissionWith: aTransmittedObject [

	self postTransmission ifNil: [ ^ self ].
	self postTransmission 
		cull: self toPresenter 
		cull: self fromPresenter 
		cull: aTransmittedObject
]

{ #category : #private }
SpTransmission >> applyPreTransmissionWith: aTransmittedObject [

	self preTransmission ifNil: [ ^ self ].
	self preTransmission 
		cull: self toPresenter 
		cull: self fromPresenter 
		cull: aTransmittedObject
]

{ #category : #actions }
SpTransmission >> do: aBlock [
	"Will create a generic action block to handle transmission"
	
	self toPort: (SpActionPort newAction: aBlock)
]

{ #category : #private }
SpTransmission >> from: aPresenter [
	self fromPort: aPresenter outputPortDefault
]

{ #category : #script }
SpTransmission >> from: aPresenter port: aSymbol [
	self fromPort: (aPresenter outputPortNamed: aSymbol)
]

{ #category : #actions }
SpTransmission >> from: aPresenter port: aSymbol to: anotherPresenter transform: aValuable [
	self
		from: aPresenter
		port: aSymbol
		to: anotherPresenter
		transform: aValuable
		postTransmission: nil
]

{ #category : #actions }
SpTransmission >> from: aPresenter port: aSymbol to: anotherPresenter transform: aValuable postTransmission: anotherValuable [

	self
		from: aPresenter port: aSymbol;
		to: anotherPresenter;
		transform: aValuable;
		postTransmission: anotherValuable;
		apply
]

{ #category : #actions }
SpTransmission >> from: aPresenter to: anotherPresenter [
	
	self
		from: aPresenter
		to: anotherPresenter
		transform: nil
]

{ #category : #actions }
SpTransmission >> from: aPresenter to: anotherPresenter transform: aValuable [
	
	self
		from: aPresenter
		to: anotherPresenter
		transform: aValuable
		postTransmission: nil
]

{ #category : #actions }
SpTransmission >> from: aPresenter to: anotherPresenter transform: aValuable postTransmission: anotherValuable [
	self
		from: aPresenter;
		to: anotherPresenter;
		transform: aValuable;
		postTransmission: anotherValuable;
		apply
]

{ #category : #accessing }
SpTransmission >> fromPort [

	^ fromPort
]

{ #category : #private }
SpTransmission >> fromPort: aPort [
	fromPort := aPort
]

{ #category : #private }
SpTransmission >> fromPresenter [
	^ self fromPort presenter
]

{ #category : #accessing }
SpTransmission >> postTransmission [
	^ postTransmission
]

{ #category : #accessing }
SpTransmission >> postTransmission: aValuable [
	postTransmission := aValuable
]

{ #category : #accessing }
SpTransmission >> preTransmission [
	^ preTransmission
]

{ #category : #accessing }
SpTransmission >> preTransmission: aValuable [
	preTransmission := aValuable
]

{ #category : #private }
SpTransmission >> to: aPresenter [

	self toPort: aPresenter inputPortDefault
]

{ #category : #script }
SpTransmission >> to: aPresenter port: aSymbol [

	self toPort: (aPresenter inputPortNamed: aSymbol)
]

{ #category : #accessing }
SpTransmission >> toPort [

	^ toPort
]

{ #category : #private }
SpTransmission >> toPort: aPort [
	toPort := aPort
]

{ #category : #private }
SpTransmission >> toPresenter [
	^ self toPort presenter
]

{ #category : #script }
SpTransmission >> transform: aBlock [

	transformBlock := aBlock
]

{ #category : #private }
SpTransmission >> transformed: anObject [ 

	transformBlock ifNil: [ ^ anObject ].
	^ transformBlock value: anObject
]
