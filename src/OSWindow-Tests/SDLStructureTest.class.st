Class {
	#name : 'SDLStructureTest',
	#superclass : 'TestCase',
	#category : 'OSWindow-Tests-Tests',
	#package : 'OSWindow-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
SDLStructureTest >> testClassVariablesAreInitialized [

	SDL2Structure withAllSubclassesDo: [ :class | class classVariables do: [ :variable | self assert: variable value isNotNil ] ]
]
