"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfSUnit

Load type: 
- linear

Installs:
- Graphics-* packages (Form, Bitmap, GrafPort...)
"
Class {
	#name : 'BaselineOfDisplay',
	#superclass : 'BaselineOf',
	#instVars : [
		'initializersEnabled'
	],
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfDisplay',
	#package : 'BaselineOfDisplay'
}

{ #category : 'baseline' }
BaselineOfDisplay >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec preLoadDoIt: #preload:package:.
			spec postLoadDoIt: #postload:package:.

			spec
				package: 'Colors';
				package: 'Graphics-Display Objects';
				package: 'Graphics-Primitives' with: [ spec requires: 'Graphics-Display Objects' ];
				package: 'Graphics-Transformations' ]
]

{ #category : 'actions' }
BaselineOfDisplay >> postload: loader package: packageSpec [
	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	MCMethodDefinition initializersEnabled: initializersEnabled.

	Initialized := true
]

{ #category : 'actions' }
BaselineOfDisplay >> preload: loader package: packageSpec [
	"Ignore pre and post loads if already executed"

	Initialized = true ifTrue: [ ^ self ].

	"For now we ensure the class initialization but in the future it should be true by default"
	initializersEnabled := MCMethodDefinition initializersEnabled.
	MCMethodDefinition initializersEnabled: true
]

{ #category : 'accessing' }
BaselineOfDisplay >> project [
	"We currently have cyclic dependencies so we are loading this project atomically."

	^ super project
		  loadType: #atomic;
		  yourself
]
