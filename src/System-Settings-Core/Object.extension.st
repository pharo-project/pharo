Extension { #name : #Object }

{ #category : #'*System-Settings-Core' }
Object >> acceptSettings: aVisitor [ 
	^ aVisitor visitObject: self
]
