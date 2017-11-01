Extension { #name : #TClass }

{ #category : #'*System-Settings' }
TClass >> acceptSettings: aVisitor [ 
	^ aVisitor visitClass: self
]
