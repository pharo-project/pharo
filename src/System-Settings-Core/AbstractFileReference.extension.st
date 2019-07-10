Extension { #name : #AbstractFileReference }

{ #category : #'*System-Settings-Core' }
AbstractFileReference >> acceptSettings: aVisitor [
	^ aVisitor visitAbstractFileReference: self
]
