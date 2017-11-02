Extension { #name : #RGElementDefinition }

{ #category : #'*Epicea-Ring' }
RGElementDefinition >> asEpiceaRingDefinition [

	^ self shallowCopy
		epiceaCleanUp;
		yourself
]

{ #category : #'*Epicea-Ring' }
RGElementDefinition >> epiceaCleanUp [

	parent := nil.

]
