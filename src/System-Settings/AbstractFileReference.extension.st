Extension { #name : #AbstractFileReference }

{ #category : #'*System-Settings' }
AbstractFileReference >> acceptSettings: aVisitor [
	^ aVisitor visitAbstractFileReference: self
]
