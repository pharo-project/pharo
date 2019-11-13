Extension { #name : #Symbol }

{ #category : #'*Slot-Core' }
Symbol >> => aVariable [
	"If the slot we give as argument is not present in the image, we will get a nil. In that case we should throw an explicit error to the user saying a slot is missing."

	aVariable ifNil: [ SlotNotFound signalForName: self ].
	^ aVariable named: self
]

{ #category : #'*Slot-Core' }
Symbol >> asClassVariable [
	^ ClassVariable named: self.
]

{ #category : #'*Slot-Core' }
Symbol >> asSlot [
	^ InstanceVariableSlot named: self.
]

{ #category : #'*Slot-Core' }
Symbol >> isPseudovariableName [
	"Answer true if I am a pseudo-variable name.
	#self isPseudovariableName -> true
	"
	^ self class pseudovariablesNames includes: self
]

{ #category : #'*Slot-Core' }
Symbol class >> pseudovariablesNames [
	^#('self' 'true' 'false' 'nil' 'thisContext' 'super')
]
