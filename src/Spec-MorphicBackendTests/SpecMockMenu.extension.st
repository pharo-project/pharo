Extension { #name : #SpecMockMenu }

{ #category : #'*Spec-MorphicBackendTests' }
SpecMockMenu >> invokeModal [

	self shown: true
]

{ #category : #'*Spec-MorphicBackendTests' }
SpecMockMenu >> isInWorld [
	
	^ false
]

{ #category : #'*Spec-MorphicBackendTests' }
SpecMockMenu >> popUpEvent: evt in: aWorld [

	self shown: true
]

{ #category : #'*Spec-MorphicBackendTests' }
SpecMockMenu >> popUpInWorld: aWorld [

	self shown: true
]

{ #category : #'*Spec-MorphicBackendTests' }
SpecMockMenu >> setInvokingView: aRubTextEditor [
]
