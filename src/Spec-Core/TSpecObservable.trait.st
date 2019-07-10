Trait {
	#name : #TSpecObservable,
	#category : #'Spec-Core-Observable'
}

{ #category : #events }
TSpecObservable >> notifyPropertyChanged: aName [

	(self observablePropertyNamed: aName) valueChanged
]

{ #category : #events }
TSpecObservable >> observablePropertyNamed: aName [
	| slot |
	
	slot := self class slotNamed: aName.
	slot isObservableSlot
		ifFalse: [ SpecNonObservableSlotError signal: aName ].

	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	^ slot rawRead: self
]

{ #category : #events }
TSpecObservable >> property: aName whenChangedDo: aBlockClosure [ 
	
	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	(self observablePropertyNamed: aName) whenChangedDo: aBlockClosure
]
