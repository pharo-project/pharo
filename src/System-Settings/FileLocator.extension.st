Extension { #name : #FileLocator }

{ #category : #'*System-Settings' }
FileLocator >> acceptSettings: aVisitor [
	^ aVisitor visitFileLocator: self
]
