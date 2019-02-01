Extension { #name : #RelativePath }

{ #category : #'*System-Settings-Core' }
RelativePath >> acceptSettings: aVisitor [
	^ aVisitor visitRelativePath: self
]
