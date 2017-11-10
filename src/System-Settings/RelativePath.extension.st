Extension { #name : #RelativePath }

{ #category : #'*System-Settings' }
RelativePath >> acceptSettings: aVisitor [
	^ aVisitor visitRelativePath: self
]
