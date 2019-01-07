Extension { #name : #FileLocator }

{ #category : #'*System-Settings-Core' }
FileLocator >> acceptSettings: aVisitor [
	^ aVisitor visitFileLocator: self
]
