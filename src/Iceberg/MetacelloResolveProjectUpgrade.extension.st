Extension { #name : #MetacelloResolveProjectUpgrade }

{ #category : #'*Iceberg-Metacello' }
MetacelloResolveProjectUpgrade >> projectName [
	
	^ self existingProjectRegistration projectName
]
