Extension { #name : #Class }

{ #category : #'*System-Settings-Core' }
Class >> acceptSettings: aVisitor [
	^ aVisitor visitClass: self
]
