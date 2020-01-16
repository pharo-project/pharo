"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfUI

Load type: 
- atomic

Installs:
- SUnit UI
- BlueInk
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
	#name : #BaselineOfBasicTools,
	#superclass : #BaselineOf,
	#classVars : [
		'Initialized'
	],
	#category : #BaselineOfBasicTools
}

{ #category : #baseline }
BaselineOfBasicTools >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.
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
			baseline: 'Athens' with: [ 
				spec
					loads: 'Cairo-core';
					repository: repository ];
			baseline: 'SUnit' with: [ 
			spec
				loads: #( 'UI' 'Help' 'JenkinsSupport' );
				repository: repository ].
		spec package: 'Tool-ExternalBrowser'.
		spec package: 'BlueInk-Core'.
		spec package: 'BlueInk-Extras'.

		spec 
			baseline: 'EnlumineurFormatter' with: [ 
				spec
					loads: 'UI';
					repository: repository ].
		
		spec package: 'Tool-ProcessBrowser'.
		spec package: 'Tool-Profilers'.
		spec package: 'Tool-SystemReporter'.
		spec package: 'NECompletion'.
		spec package: 'Announcements-Help'.
		spec package: 'Metacello-FileTree'.
		spec package: 'Metacello-Cypress'.
		spec package: 'Metacello-ProfStef'.
		spec package: 'Metacello-Reference'.
		spec package: 'Metacello-Tutorial'.
		spec package: 'MonticelloGUI'.
		spec package: 'Tests'.
		spec package: 'RPackage-Tests'.
		spec package: 'Monticello-Tests'.
		spec package: 'MonticelloGUI-Tests'.
		spec package: 'Network-Mail'.
		spec package: 'Network-Mail-Tests'.
		spec package: 'Pharo-Help'.
		spec package: 'ProfStef-Core'.
		spec package: 'ProfStef-Help'.
		spec package: 'RecentSubmissions'.
		spec package: 'Refactoring-Changes'.
		spec package: 'Refactoring-Core'.
		spec package: 'Refactoring-Environment'.
		spec package: 'Regex-Help'.
		spec package: 'IssueTracking'.
		spec package: 'Monticello-OldDataStreamCompatibility'.
		spec package: 'MonticelloFileServices'.
		spec package: 'Spec2-Help'.
		spec package: 'Tool-Diff'.
		spec package: 'Tool-FileList'.
		spec package: 'Tool-Finder'.
		spec package: 'Tool-ImageCleaner'.
		spec package: 'Tools' ]
]

{ #category : #actions }
BaselineOfBasicTools >> postload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].
	
	Smalltalk tools register: TestRunner as: #testRunner.
	Smalltalk tools register: MCWorkingCopyBrowser as: #monticelloBrowser.
	
	RBRefactoryChangeManager nuke.
	Smalltalk garbageCollect.
	Smalltalk cleanOutUndeclared. 
	
	CompletionSorter register.
	RubSmalltalkEditor completionEngineClass: CompletionEngine.
	
	Author reset.

	Initialized := true.
]

{ #category : #baseline }
BaselineOfBasicTools >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
	
]
