Extension { #name : #RFiMethod }

{ #category : #'*Hermes-Ring' }
RFiMethod >> acceptVisitor: aVisitor [
	^ aVisitor visitMethod: self. 
	
]
