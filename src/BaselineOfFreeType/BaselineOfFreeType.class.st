Class {
	#name : 'BaselineOfFreeType',
	#superclass : 'BaselineOf',
	#instVars : [
		'initializersEnabled'
	],
	#category : 'BaselineOfFreeType',
	#package : 'BaselineOfFreeType'
}

{ #category : 'baselines' }
BaselineOfFreeType >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec package: 'FreeType'.
		spec package: 'FreeType-Graphics' with: [ spec requires: #('FreeType') ].		
		spec package: 'FreeType-Tests' with: [ spec requires: #('FreeType') ].

		spec group: 'Core' with: #('FreeType' 'FreeType-Graphics').
		spec group: 'tests' with: #('core' 'FreeType-Tests') ]
]

{ #category : 'actions' }
BaselineOfFreeType >> postload: loader package: packageSpec [

	MCMethodDefinition initializersEnabled: initializersEnabled
]

{ #category : 'actions' }
BaselineOfFreeType >> preload: loader package: packageSpec [
	"For now we ensure the class initialization but in the future it should be true by default"

	initializersEnabled := MCMethodDefinition initializersEnabled.
	MCMethodDefinition initializersEnabled: true
]

{ #category : 'accessing' }
BaselineOfFreeType >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject
]
