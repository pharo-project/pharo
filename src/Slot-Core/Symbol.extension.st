Extension { #name : #Symbol }

{ #category : #'*Slot-Core' }
Symbol >> => aVariable [
	aVariable isBehavior
		ifTrue: [ ^ aVariable named: self].
	^ aVariable name: self;
		yourself
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
