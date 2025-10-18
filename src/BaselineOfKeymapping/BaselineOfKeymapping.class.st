Class {
	#name : 'BaselineOfKeymapping',
	#superclass : 'BaselineOf',
	#instVars : [
		'initializersEnabled'
	],
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfKeymapping',
	#package : 'BaselineOfKeymapping'
}

{ #category : 'baselines' }
BaselineOfKeymapping >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec preLoadDoIt: #preload:package:.
			spec postLoadDoIt: #postload:package:.
			
			spec
				package: 'Keymapping-Core';
				package: 'Keymapping-KeyCombinations';
				package: 'Keymapping-Pragmas';
				package: 'Keymapping-Settings';
				package: 'Keymapping-Tests'.

			spec
				group: 'default' with: #( 'core' 'ui' 'tests' );
				group: 'core' with: #( 'Keymapping-Core' 'Keymapping-KeyCombinations' );
				group: 'morphic' with: #( 'core' 'Keymapping-Pragmas' 'Keymapping-Settings' );
				group: 'ui' with: #( 'core' 'morphic' );
				group: 'tests' with: #( 'core' 'Keymapping-Tests' ) ]
]

{ #category : 'actions' }
BaselineOfKeymapping >> postload: loader package: packageSpec [
	"Ignore pre and post loads if already executed"

	Initialized = true ifTrue: [ ^ self ].

	MCMethodDefinition initializersEnabled: initializersEnabled
]

{ #category : 'actions' }
BaselineOfKeymapping >> preload: loader package: packageSpec [
	"Ignore pre and post loads if already executed"

	Initialized = true ifTrue: [ ^ self ].

	initializersEnabled := MCMethodDefinition initializersEnabled.
	MCMethodDefinition initializersEnabled: true
]

{ #category : 'accessing' }
BaselineOfKeymapping >> project [
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.

	^ aProject
]
