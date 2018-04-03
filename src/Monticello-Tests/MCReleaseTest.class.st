Class {
	#name : #MCReleaseTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests-Base'
}

{ #category : #tests }
MCReleaseTest >> testMonticelloInitializersAreEnabledByDefault [

	"Monticello should by default have this option enabled.
	If this is not the case, the image cannot be released, as it will break users that expect class initializations to be run automatically"

	self assert: MCMethodDefinition initializersEnabled.
]
