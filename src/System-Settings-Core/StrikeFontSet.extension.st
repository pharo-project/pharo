Extension { #name : #StrikeFontSet }

{ #category : #'*System-Settings-Core' }
StrikeFontSet >> acceptSettings: aVisitor [
	^ aVisitor visitStrikeFontSet: self
]
