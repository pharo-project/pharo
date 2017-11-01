Extension { #name : #Trait }

{ #category : #'*System-Settings' }
Trait >> acceptSettings: aVisitor [ 
	^ aVisitor visitClass: self
]
