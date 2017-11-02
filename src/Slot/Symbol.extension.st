Extension { #name : #Symbol }

{ #category : #'*Slot' }
Symbol >> => aVariable [
	aVariable isBehavior
		ifTrue: [ ^ aVariable named: self].
	^ aVariable name: self;
		yourself
]

{ #category : #'*Slot' }
Symbol >> asClassVariable [
	^ ClassVariable named: self.
]

{ #category : #'*Slot' }
Symbol >> asSlot [
	^ InstanceVariableSlot named: self.
]

{ #category : #'*Slot' }
Symbol >> isPseudovariableName [
	"Answer true if I am a pseudo-variable name.
	#self isPseudovariableName -> true
	"
	^ self class pseudovariablesNames includes: self
]

{ #category : #'*Slot' }
Symbol class >> pseudovariablesNames [
	^#('self' 'true' 'false' 'nil' 'thisContext' 'super')
]
