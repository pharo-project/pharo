"
I am a trait allowing one to interact with an ObservableSlot. 

An ObservableSlot allows one to register actions that will be executed when the content of a variable change.

Public API and Key Messages
--------------------

- #property:whenChangedDo: 		With this method one can declare than when the value in a given observable slot change, an action must be executed. 
- #property:rawValue: 			With this method a value can be writen without launching any action.

Examples
--------------------

	| count |
	count := 0.
	point := ObservablePoint new.
	point property: #x whenChangedDo: [ count := count + 1 ].

	point x: 17.

	self assert: count equals: 1


"
Trait {
	#name : #TObservable,
	#category : #'VariablesLibrary-Observable'
}

{ #category : #events }
TObservable >> notifyPropertyChanged: aName [
	self flag: #pharoTodo.	"This is used for collections but collections should be managed in a better way and this method removed."
	(self observablePropertyNamed: aName) valueChanged
]

{ #category : #events }
TObservable >> observablePropertyNamed: aName [
	| slot |
	slot := self class slotNamed: aName.
	slot isObservable ifFalse: [ NonObservableSlotError signal: aName ].

	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	^ slot rawRead: self
]

{ #category : #events }
TObservable >> property: aName rawValue: anObject [
	"Write in the slot without announcing it."

	(self observablePropertyNamed: aName) rawValue: anObject
]

{ #category : #events }
TObservable >> property: aName whenChangedDo: aBlockClosure [ 
	
	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	(self observablePropertyNamed: aName) whenChangedDo: aBlockClosure
]
