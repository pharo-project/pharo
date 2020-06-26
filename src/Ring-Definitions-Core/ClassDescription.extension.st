Extension { #name : #ClassDescription }

{ #category : #'*Ring-Definitions-Core' }
ClassDescription >> methodsInProtocol: aString [

	^ (self organization listAtCategoryNamed: aString) 
			collect: [ :each | (self compiledMethodAt: each) ]
]
