Extension { #name : #ClassVariable }

{ #category : #'*Reflectivity' }
ClassVariable >> astNodes [
	^self definingClass variableNodes select: [ :each | each binding variable == self ]
]
