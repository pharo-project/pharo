"
! What is a transmission?
Transmissions are a way to connect presenters, thinking on the ""flow"" of information more than the way it is displayed. For example, think on a master-detail (A->B) relationship, when you navigate the elements in master A, you want to see the detail B. This is tipically solved showing a list with master elements and a form with the detail of each master. 
In Spec, this will be declared more or less like this: 

[[[language=Smalltalk
layout := SpBoxLayout newHorizontal
	add: (list := self newList);
	add: (detail := self newText);
	yourself.	
]]]

But this does not says how ==list== and ==detail== are linked. 

The transmission sub-framework solves this in an elegant way: Each presenter defines ''output ports'' (ports to send information) and ''input ports'' (ports to receive information). Each presenter defines also a default input port and a default output port.

!! Transmitting from an output port to an input port (==#transmitTo:==)

A transmission connects a presenter's output port with a presenter's input port. 
See this example: 

[[[language=Smalltalk
list transmitTo: detail.
]]]

This will connect the ==list== presenter """"default output port"""" with the ==detail== presenter """"default input port"""". This line is equivallent (but a lot simpler) to this one:  

[[[language=Smalltalk
list defaultOutputPort transmitTo: detail defaultInputPort
]]]

Is important to remark that a transmission does not connects two components, it connect two """"component ports"""". The distinction is important because there can be many ports!  
Take for example *class:SpListPresenter*, it defines two output ports (selection and activation), this means it is possible to define also this transmission: 

[[[language=Smalltalk
list outputActivationPort transmitTo: detail defaultInputPort
]]]

!! Transforming a transmission (==#transmitTo:transform:==)
The object transmitted from a presenter output port can be inadequate for the input port. To solve this problem a transmission allow transformations.
This is as simple as using the ==#transform:== protocol: 

[[[language=Smalltalk
list 
	transmitTo: detail 
	transform: [ :aValue | aValue asString ].
]]]

[[[language=Smalltalk
list defaultOutputPort 
	transmitTo: detail defaultInputPort 
	transform: [ :aValue | aValue asString ].
]]]

!! Transmitting from an output port to an arbitrary input receiver (==#transmitDo:==, ==#transmitDo:transform:==)
It is possible that the user requires to listen an output port, but instead transmitting the value to another presenter, other operation is needed. 
There is the ==#transmitDo:== protocol to handle this situation: 

[[[language=Smalltalk
list transmitDo: [ :aValue | aValue crTrace ].
]]]

!! Acting after a transmission (==#postTransmission:==)
Sometimes after a transmission happens, the user needs to react to modify something given the new status achieved by the presenter (like, pre-selecting something).
The ==#postTransmission:== protocol allows you to handle that situation.

[[[language=Smalltalk
list 
	transmitTo: detail 
	postTransmission: [ :fromPresenter :toPresenter :value | 
		""something to do here""
		toPresenter enabled: value isEmptyOrNil not ].
]]]

See *method:SpTransmission#postTransmission:*
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
	self fromPort: aPresenter defaultOutputPort
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

	self toPort: aPresenter defaultInputPort
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
