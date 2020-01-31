Trait {
	#name : #SpTObservable,
	#category : #'Spec2-ObservableSlot'
}

{ #category : #events }
SpTObservable >> notifyPropertyChanged: aName [
	self flag: #todo.	"This is used for collections but collections should be managed in a better way and this method removed."
	(self observablePropertyNamed: aName) valueChanged
]

{ #category : #events }
SpTObservable >> observablePropertyNamed: aName [
	| slot |
	
	slot := self class slotNamed: aName.
	slot isObservableSlot
		ifFalse: [ SpNonObservableSlotError signal: aName ].

	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	^ slot rawRead: self
]

{ #category : #events }
SpTObservable >> property: aName rawValue: anObject [
	"Write in the slot without announcing it."

	(self observablePropertyNamed: aName) rawValue: anObject
]

{ #category : #events }
SpTObservable >> property: aName whenChangedDo: aBlockClosure [ 
	
	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	(self observablePropertyNamed: aName) whenChangedDo: aBlockClosure
]
