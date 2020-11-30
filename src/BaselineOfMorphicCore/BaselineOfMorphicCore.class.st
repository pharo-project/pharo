"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- SUnit, Display, UnifiedFFI

Load type: 
- linear

Installs:
- minimal working Morphic subset without fonts support
"
Class {
	#name : #BaselineOfMorphicCore,
	#superclass : #BaselineOf,
	#classVars : [
		'Initialized'
	],
	#category : #BaselineOfMorphicCore
}

{ #category : #baseline }
BaselineOfMorphicCore >> baseline: spec [ 
    
	<baseline>

	| repository | 
	
	repository := self packageRepositoryURL.

	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.

		spec baseline: 'Traits' with: [spec repository: repository].
		spec baseline: 'SUnit' with: [spec repository: repository].
		spec baseline: 'Display' with: [spec repository: repository].
		spec baseline: 'UnifiedFFI' with: [spec repository: repository].

		spec package: 'System-VMEvents'.
		spec package: 'System-Object Events'.
		spec package: 'Collections-DoubleLinkedList'.
		spec package: 'System-Caching'.
		spec package: 'Graphics-Canvas'.
		spec package: 'FormCanvas-Core'.
		spec package: 'Tool-Registry'.
		spec package: 'Fonts-Abstract'.
		spec package: 'Text-Core'.
		spec package: 'Morphic-Core'.
	].
]

{ #category : #actions }
BaselineOfMorphicCore >> postload: loader package: packageSpec [

	| world hand |

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].
	
	Stdio stdout nextPutAll: 'Initializing Morphic 1...'; lf.

	IdentityTransform initialize.

	EventSensorConstants initialize.

	RealEstateAgent initialize.
	WorldState initialize.
	Morph initialize.
	HandMorph initialize.
	
	AbstractWorldRenderer initialize.	

	Cursor initTarget.
	EventManager initialize.

	Stdio stdout nextPutAll: 'Initializing Morphic 2...'; lf.

	world := WorldMorph new.
	world instVarNamed: #worldState put: WorldState new.
	hand := HandMorph new.
	world addHand: hand.
	world activeHand: hand.

	Smalltalk at: #World put: world.
	Smalltalk at: #ActiveWorld put: world.

	world viewBox: Display boundingBox.
	
	Stdio stdout nextPutAll: 'Initializing Morphic 3...'; lf.

	Smalltalk at: #Sensor put: InputEventSensor new.
	InputEventSensor installEventSensorFramework.
	InputEventSensor installMouseDecodeTable.
	
	Stdio stdout nextPutAll: 'Initializing Morphic 3.1...'; lf.

	(Smalltalk at: #Sensor) flushAllButDandDEvents.
	world handsDo: [:h | h initForEvents].
	world borderWidth: 0.
	
	Stdio stdout nextPutAll: 'Initializing Morphic 3.2...'; lf.

	Display newDepth: 32.
	world displayWorldSafely.
	MorphicCoreUIManager new spawnNewProcess.
	
	Stdio stdout nextPutAll: 'Initializing Morphic 4...'; lf.

	world cleanseOtherworldlySteppers.
	ExternalDropHandler resetRegisteredHandlers.

	Display newDepth: 32.

	(world instVarNamed: #worldState) invalidate.
	
	Stdio stdout nextPutAll: 'Initializing Morphic 5...'; lf.

	hand instVarNamed: #targetOffset put: 0@0.

	WorldMorph initialize.
	ExternalDropHandler initialize.
	PasteUpMorph initialize.
	DefaultExternalDropHandler initialize.
	
	WorldMorph recompile.
	
	Stdio stdout nextPutAll: 'Initializing Morphic 6...'; lf.

	Initialized := true.
]
