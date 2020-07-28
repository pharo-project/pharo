Class {
	#name : #StPharoApplication,
	#superclass : #SpApplication,
	#classVars : [
		'Current'
	],
	#category : #'NewTools-Core-Application'
}

{ #category : #'instance creation' }
StPharoApplication class >> current [ 
	
	^ Current ifNil: [ Current := self new ]
]

{ #category : #'class initialization' }
StPharoApplication class >> initialize [ 

	SessionManager default registerToolClassNamed: self name
]

{ #category : #'system startup' }
StPharoApplication class >> shutDown: quitting [

	Current ifNil: [ ^ self ].
	Current shutDown: quitting
]

{ #category : #'system startup' }
StPharoApplication class >> startUp: resuming [

	Current ifNil: [ ^ self ].
	Current startUp: resuming
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

{ #category : #'system startup' }
StPharoApplication >> shutDown: quitting [
]

{ #category : #'private running' }
StPharoApplication >> start [

	
]

{ #category : #'system startup' }
StPharoApplication >> startUp: resuming [
]

{ #category : #settings }
StPharoApplication >> toolbarDisplayMode [

	^ StPharoSettings toolbarDisplayMode
]
