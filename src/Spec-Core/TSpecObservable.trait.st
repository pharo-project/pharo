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
	
	(self class slotNamed: aName) isObservableSlot
		ifFalse: [ SpecNonObservableSlotError signal: aName ].

	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	^ self instVarNamed: aName
]

{ #category : #events }
TSpecObservable >> property: aName whenChangedDo: aBlockClosure [ 
	
	"Obtain the raw value.
	We need to access the underlying value holder to subscribe to it"
	(self observablePropertyNamed: aName) whenChangedDo: aBlockClosure
]
