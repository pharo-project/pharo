Extension { #name : #Class }

{ #category : #'*System-Settings' }
Class >> acceptSettings: aVisitor [ 
	^ aVisitor visitClass: self
]
