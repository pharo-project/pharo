Extension { #name : #AbsolutePath }

{ #category : #'*System-Settings-Core' }
AbsolutePath >> acceptSettings: aVisitor [
	^ aVisitor visitAbsolutePath: self
]
