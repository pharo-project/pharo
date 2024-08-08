"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfUI

Load type: 
- atomic

Installs:
- SUnit UI
- Nautilus
- NECompletion
- ProcessBrowsers
- profillers
- different Metacello repositories
- Monticello GUI
- ProfStef
- Athens
..and some other packages
"
Class {
	#name : 'BaselineOfBasicTools',
	#superclass : 'BaselineOf',
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfBasicTools',
	#package : 'BaselineOfBasicTools'
}

{ #category : 'baselines' }
BaselineOfBasicTools >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		spec postLoadDoIt: #postload:package:.

		"Load first SUnit core"
		spec baseline: 'SUnitCore' with: [
			spec
				loads: 'Core';
				className: 'BaselineOfSUnit';
				repository: repository ].
		spec
			baseline: 'UI' with: [ spec repository: repository ];
			baseline: 'Reflectivity' with: [ spec repository: repository ];
			baseline: 'DebugPoints' with: [ spec repository: repository ];
			baseline: 'Athens' with: [
				spec
					loads: 'Cairo-core';
					repository: repository ];
			baseline: 'SUnit' with: [
			spec
				loads: #( 'UI' 'JenkinsSupport' );
				repository: repository ].
		spec package: 'Tool-ExternalBrowser'.
		spec baseline: 'EnlumineurFormatter' with: [
			spec
				loads: 'Core';
				repository: repository ].

		spec package: 'Tool-ProcessBrowser'.
		spec package: 'Tool-Profilers'.
		spec package: 'NECompletion'.
		spec package: 'NECompletion-Morphic'.
		spec package: 'NECompletion-Preferences'.
		spec package: 'Metacello-FileTree'.
		spec package: 'Metacello-Cypress'.
		spec package: 'CodeExport'.
		spec package: 'MonticelloGUI' with: [ spec requires: #( 'Tool-Diff' ) ].
		spec package: 'System-Sources-Tests'. "<= Not sure this one should be here but it is where the classes were loaded before been extracted from Tests package."

		spec package: 'Kernel-CodeModel-Tests'.
		spec package: 'Monticello-Tests'.
		spec package: 'MonticelloGUI-Tests'.
		spec package: 'Network-Mail'.
		spec package: 'Network-Mail-Tests'.
		spec package: 'ProfStef-Core'.
		spec package: 'StartupPreferences'.

		spec baseline: 'Refactoring' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].

		spec package: 'Monticello-BackwardCompatibility'.
		spec package: 'MonticelloFileServices'.

		self specRefactoring: spec.

		spec package: 'Tools-CodeNavigation'.
		spec package: 'Tool-Diff'.
		spec package: 'Tool-FileList'.
		spec package: 'Tool-Finder'.
		spec package: 'Tool-Finder-UI'.
		spec package: 'Tool-ImageCleaner'.
		spec package: 'Tools' ]
]

{ #category : 'actions' }
BaselineOfBasicTools >> postload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].
	
	CompletionSorter register.
	RubSmalltalkEditor completionEngineClass: CompletionEngine.

	Initialized := true.
]

{ #category : 'baseline' }
BaselineOfBasicTools >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
]

{ #category : 'baseline' }
BaselineOfBasicTools >> specRefactoring: spec [
	
	"Package used to rename #defaultSpec methods to #defaultLayout.
	Should be removed once users of #defaultSpec are migrated."
	
	spec baseline: 'SpecRefactoring' with: [
		spec 
			repository: (self class environment at: #BaselineOfPharo) specRepository;
			className: 'BaselineOfSpec2';
			loads: #('Spec2-Transformations') ]
]
