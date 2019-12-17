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
		spec package: 'Graphics-Canvas'.
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

	IdentityTransform initialize.

	EventSensorConstants initialize.

	RealEstateAgent initialize.
	WorldState initialize.
	Morph initialize.
	HandMorph initialize.
	
	AbstractWorldRenderer initialize.	

	Cursor initTarget.
	EventManager initialize.

	world := WorldMorph new.
	world instVarNamed: #worldState put: WorldState new.
	hand := HandMorph new.
	world addHand: hand.
	world activeHand.

	Smalltalk at: #World put: world.
	Smalltalk at: #ActiveWorld put: world.
	Smalltalk at: #ActiveHand put: hand.
	Smalltalk at: #ActiveEvent put: nil.

	world viewBox: Display boundingBox.

	Smalltalk at: #Sensor put: InputEventSensor new.
	InputEventSensor installEventSensorFramework.
	InputEventSensor installMouseDecodeTable.

	(Smalltalk at: #Sensor) flushAllButDandDEvents.
	world world handsDo: [:h | h initForEvents].
	world borderWidth: 0.

	MorphicCoreUIManager new spawnNewProcess.
	Display newDepth: 32.
	world displayWorldSafely.
	UIManager default: MorphicCoreUIManager new.
	world displayWorldSafely.
	UIManager default uiProcess resume.

	World cleanseOtherworldlySteppers.
	ExternalDropHandler resetRegisteredHandlers.

	Display newDepth: 32.

	(world instVarNamed: #worldState) canvas: nil.

	hand instVarNamed: #targetOffset put: 0@0.

	WorldMorph initialize.
	ExternalDropHandler initialize.
	PasteUpMorph initialize.
	DefaultExternalDropHandler initialize.
	
	WorldMorph recompile.

	Initialized := true.
]
