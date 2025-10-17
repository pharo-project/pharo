Class {
	#name : 'BaselineOfFreeType',
	#superclass : 'BaselineOf',
	#instVars : [
		'initializersEnabled'
	],
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfFreeType',
	#package : 'BaselineOfFreeType'
}

{ #category : 'baselines' }
BaselineOfFreeType >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec preLoadDoIt: #preload:package:.
			spec postLoadDoIt: #postload:package:.

			spec package: 'FreeType'.
			spec package: 'FreeType-Graphics' with: [ spec requires: #( 'FreeType' ) ].
			spec package: 'FreeType-Morphic' with: [ spec requires: #( 'FreeType-Graphics' ) ].
			spec package: 'FreeType-Tests' with: [ spec requires: #( 'FreeType' ) ].

			spec group: 'core' with: #( 'FreeType' ).
			spec group: 'ui' with: #( 'core' 'FreeType-Graphics' 'FreeType-Morphic' ).
			spec group: 'tests' with: #( 'core' 'FreeType-Tests' ) ]
]

{ #category : 'actions' }
BaselineOfFreeType >> postload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	MCMethodDefinition initializersEnabled: initializersEnabled.
]

{ #category : 'actions' }
BaselineOfFreeType >> preload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

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
