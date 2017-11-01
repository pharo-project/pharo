Extension { #name : #AbsolutePath }

{ #category : #'*System-Settings' }
AbsolutePath >> acceptSettings: aVisitor [
	^ aVisitor visitAbsolutePath: self
]
