Extension { #name : #Symbol }

{ #category : #'*Slot-Core' }
Symbol >> => aVariable [
	"If the slot we give as argument is not present in the image, we will get a nil. In that case we should throw an explicit error to the user saying a slot is missing."

	aVariable ifNil: [ SlotNotFound signalForName: self ].
	^ aVariable named: self
]
