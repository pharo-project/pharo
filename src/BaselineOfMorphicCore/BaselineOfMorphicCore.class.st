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
	#name : 'BaselineOfMorphicCore',
	#superclass : 'BaselineOf',
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfMorphicCore',
	#package : 'BaselineOfMorphicCore'
}

{ #category : 'baseline' }
BaselineOfMorphicCore >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
		spec postLoadDoIt: #postload:package:.

		spec package: 'System-Object Events'.
		spec package: 'System-Caching'.
		spec package: 'System-Utilities'.
		spec package: 'Graphics-Canvas'.
		spec package: 'FormCanvas-Core'.
		spec package: 'Tool-Registry'.
		spec package: 'System-Model'.
		spec baseline: 'Fonts' with: [
				spec
					repository: repository;
					loads: #Core ].
		spec baseline: 'FreeType' with: [
			spec
				repository: repository;
				loads: 'ui' ].
		spec package: 'Morphic-Core' with: [ spec requires: #( 'System-Model' ) ].
		spec package: 'System-FileRegistry' with: [ spec requires: #( 'System-Model' ) ] ]
]

{ #category : 'actions' }
BaselineOfMorphicCore >> postload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	RealEstateAgent initialize.
	WorldState initialize.
	Morph initialize.
	HandMorph initialize.
	AbstractWorldRenderer initialize.	
	EventManager initialize.
	UserInputEvent initialize.
	PasteUpMorph initialize.
	WorldMorph initialize.
	DefaultExternalDropHandler initialize.
	BalloonEngineConstants initialize.
	BalloonEngine initialize.
	CornerRounder initialize.
	Clipboard initialize.
	GradientFillStyle initialize.
	TextAction initialize.
	TextConstants initialize.
	TextStyle initialize.
	PharoCommonTools initialize.

	Initialized := true.
]
