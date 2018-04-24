Extension { #name : #SequenceableCollection }

{ #category : #'*Graphics-Display Objects' }
SequenceableCollection >> asColorArray [
	^ColorArray withAll: self
]
