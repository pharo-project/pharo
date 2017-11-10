Extension { #name : #TClass }

{ #category : #'*Monticello' }
TClass >> asClassDefinition [
	^self explicitRequirement.
]

{ #category : #'*Monticello' }
TClass >> classDefinitions [
	^self explicitRequirement.
]
