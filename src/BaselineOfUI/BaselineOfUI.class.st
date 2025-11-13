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

{ #category : 'loading' }
BaselineOfUI class >> debuggerBaseUIPackageNames [

	^ self packagesOfGroupNamed: #'Debugger-Base-UI'
]

{ #category : 'baseline' }
BaselineOfUI >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		spec postLoadDoIt: #postload:package:.

		"Loading the debugger model and infrastructure before the UI"
		spec package: 'Debugger-Model'.
		spec package: 'Debugger-Oups'.

		spec baseline: 'Display' with: [ spec repository: repository ].
		spec baseline: 'Fonts' with: [
				spec
					repository: repository;
					loads: #Core ].
		spec baseline: 'FreeType' with: [
			spec
				repository: repository;
				loads: 'ui' ].
		spec baseline: 'Fuel' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].
		spec
				baseline: 'SUnit' with: [
						spec
							loads: 'UI';
							repository: repository ].
		
		"Load morphic before Spec"
		spec baseline: 'Morphic' with: [ spec repository: repository ].
		spec baseline: 'Commander2' with: [ spec repository: repository ].
		"Spec 2 (base), using external configuration"
		self specBase: spec.

		spec package: 'StartupPreferences'.
		spec package: 'Morphic-Widgets-Tree'.

		spec baseline: 'Keymapping' with: [
			spec
				repository: repository;
				loads: 'ui' ].
		spec package: 'WebBrowser-Core'.
		
		spec group: 'Debugger-Base-UI' with: #( 'Debugger-Model' 'Debugger-Oups') ]
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
