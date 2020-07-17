Class {
	#name : #StPharoApplication,
	#superclass : #SpApplication,
	#classVars : [
		'Current'
	],
	#classInstVars : [
		'session'
	],
	#category : #'NewTools-Core-Application'
}

{ #category : #'instance creation' }
StPharoApplication class >> current [ 
	
	Current ifNil: [ Current := self new ].
	self validateSession.
	^ Current
]

{ #category : #private }
StPharoApplication class >> validateSession [
	
	session == Smalltalk session ifTrue: [ ^ self ].
	Current sessionChanged.
	session := Smalltalk session
]

{ #category : #initialization }
StPharoApplication >> initialize [

	super initialize.
	self resetConfiguration
]

{ #category : #initialization }
StPharoApplication >> resetConfiguration [
	
	self flag: #TODO. "Replace this with a proper initialization mechanism (which is not
	implemented at the moment, and we need to choose the backend from it)"
	self class environment 
		at: #StPharoMorphicConfiguration
		ifPresent: [ :aClass | self useBackend: #Morphic with: aClass new ]
]

{ #category : #running }
StPharoApplication >> run [ 

	Current ifNotNil: [ Current close ].
	super run. 
	Current := self
]

{ #category : #initialization }
StPharoApplication >> sessionChanged [

	self resetConfiguration
]

{ #category : #'private running' }
StPharoApplication >> start [

	
]

{ #category : #settings }
StPharoApplication >> toolbarDisplayMode [

	^ StPharoSettings toolbarDisplayMode
]
