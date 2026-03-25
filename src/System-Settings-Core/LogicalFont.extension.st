Extension { #name : 'LogicalFont' }

{ #category : '*System-Settings-Core' }
LogicalFont >> acceptSettings: aVisitor [
	^ aVisitor visitLogicalFont: self
]
