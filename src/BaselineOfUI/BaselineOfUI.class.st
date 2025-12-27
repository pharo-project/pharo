"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- ConfigurationOfMorphic

Load type: 
- atomic

Installs:
- Spec
- Keymappings
- Rubric
- basic tools (old debugger, workspace)
- some other packages
"
Class {
	#name : 'BaselineOfUI',
	#superclass : 'BaselineOf',
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfUI',
	#package : 'BaselineOfUI'
}

{ #category : 'baseline' }
BaselineOfUI >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec repository: (self packageRepositoryURLForSpec: spec).
			spec postLoadDoIt: #postload:package:.

			spec baseline: 'SUnit' with: [ spec loads: 'UI' ].
			
			"Load morphic before Spec"
			spec baseline: 'MorphicCore'.
			spec baseline: 'Morphic'.
			spec baseline: 'Commander2'.
			
			"Spec 2 (base), using external configuration"
			self specBase: spec.

			spec package: 'Morphic-Widgets-Tree'.
			spec package: 'WebBrowser-Core' ]
]

{ #category : 'actions' }
BaselineOfUI >> postload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	Smalltalk ui theme settings preferGradientFill: true.

	Initialized := true
]

{ #category : 'baseline' }
BaselineOfUI >> specBase: spec [
	
	spec baseline: 'SpecBase' with: [
		spec 
			repository: (self class environment at: #BaselineOfPharo) specRepository;
			className: 'BaselineOfSpec2';
			loads: #('Base' 'Tests') ]
]
