Extension { #name : #GlobalVariable }

{ #category : #'*Reflectivity' }
GlobalVariable >> astNodes [
	^self usingMethods flatCollect: [ :method | 
		method variableNodes select: [: each| each binding variable == self ]]
]
