Extension { #name : #SpMockMenu }

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMockMenu >> invokeModal [

	self shown: true
]

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMockMenu >> isInWorld [
	
	^ false
]

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMockMenu >> popUpEvent: evt in: aWorld [

	self shown: true
]

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMockMenu >> popUpInWorld: aWorld [

	self shown: true
]

{ #category : #'*Spec2-Morphic-Backend-Tests' }
SpMockMenu >> setInvokingView: aRubTextEditor [
]
