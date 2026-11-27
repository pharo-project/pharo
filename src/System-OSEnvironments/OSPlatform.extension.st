Extension { #name : 'OSPlatform' }

{ #category : '*System-OSEnvironments' }
OSPlatform >> environmentVariablesDo: aBlock [
	"Nothing by default"

	
]

{ #category : '*System-OSEnvironments' }
OSPlatform >> keysAndValuesDo: aBlock withAssociationString: associationString [
	| equalsIndex |
	equalsIndex := associationString indexOf: $=.
	aBlock
		value: (associationString first: equalsIndex-1)
		value: (associationString allButFirst: equalsIndex)
]

{ #category : '*System-OSEnvironments' }
OSPlatform >> removeEnvironmentVariableNamed: key [
	"Do nothing"

	
]

{ #category : '*System-OSEnvironments' }
OSPlatform >> setEnv: nameString value: valueString [
	"We do not support environment Variables. Do Nothing"
]
