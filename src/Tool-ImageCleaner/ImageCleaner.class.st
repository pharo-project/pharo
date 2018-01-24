"
Used for cleaning the image
"
Class {
	#name : #ImageCleaner,
	#superclass : #Object,
	#category : #'Tool-ImageCleaner'
}

{ #category : #'instance creation' }
ImageCleaner class >> cleanUpForProduction [
	self new cleanUpForProduction
]

{ #category : #'instance creation' }
ImageCleaner class >> cleanUpForRelease [
	self new cleanUpForRelease
]

{ #category : #api }
ImageCleaner >> cleanUpForProduction [
	"self new cleanUpForProduction"
	"delete logo"	
	PolymorphSystemSettings showDesktopLogo: false.

	self packagesForCleanUpInProduction do: [ :each | (MCPackage named: each) unload ].	
	self helpPackages do: [ :each | (MCPackage named: each) unload ].	
	self testPackages do: [ :each | (MCPackage named: each) unload ].
	self examplePackages do: [ :each | (MCPackage named: each) unload ].
	(MCPackage named: 'ScriptLoader40') unload.	
		
		
	(MCPackage named: 'MonticelloMocks') unload.	
	(MCPackage named: 'ToolsTest') unload.	
	(MCPackage named: 'Announcements-Tests-Core') unload.	
	(MCPackage named: 'AST-Tests-Core') unload.	
	(MCPackage named: 'AST-Interpreter-Test') unload.	
	(MCPackage named: 'Ring-Tests-Containers') unload.	
	(MCPackage named: 'Ring-Tests-Kernel') unload.	
	(MCPackage named: 'Ring-Tests-Monticello') unload.	
	(MCPackage named: 'Regex-Tests-Core') unload.	
	(MCPackage named: 'Refactoring-Tests-Changes') unload.	
	(MCPackage named: 'Refactoring-Tests-Core') unload.	
	(MCPackage named: 'Refactoring-Tests-Critics') unload.	
	(MCPackage named: 'Refactoring-Tests-Environment') unload.	
	(MCPackage named: 'FileSystem-Tests-Core') unload.	
	(MCPackage named: 'FileSystem-Tests-Disk') unload.	
	(MCPackage named: 'FileSystem-Tests-Memory') unload.		
			
	(MCPackage named: 'GT-Tests-Inspector') unload.		
	(MCPackage named: 'Glamour-Tests-Core') unload.		
	(MCPackage named: 'Glamour-Tests-Morphic') unload.		
	(MCPackage named: 'Glamour-Tests-Resources') unload.		
	(MCPackage named: 'Glamour-Tests-Rubric') unload.		
			
	(MCPackage named: 'Versionner-Core-Model') unload.
	(MCPackage named: 'Versionner-Core-DependenciesModel') unload.		
	(MCPackage named: 'Versionner-Core-Announcements') unload.	
	(MCPackage named: 'Versionner-Core-Commands') unload.	
	(MCPackage named: 'Versionner-Spec-Browser') unload.				
	(MCPackage named: 'Versionner-Tests-Core-Commands') unload.	
	(MCPackage named: 'Versionner-Tests-Core-DependenciesModel') unload.	
	(MCPackage named: 'Versionner-Tests-Core-Model') unload.			
	(MCPackage named: 'Versionner-Tests-Resources') unload.												
	
	self cleanUpForRelease.
	
]

{ #category : #api }
ImageCleaner >> cleanUpForRelease [
	"self new cleanUpForRelease"

	Author fullName: 'MrCleaner'.
	self cleanUpMethods.
	
	FreeTypeFontProvider current initialize.
	EmbeddedFreeTypeFontInstaller initialize.
	self class environment at: #MetacelloProjectRegistration ifPresent: [ :class | class resetRegistry ].
	SystemNavigation new
		allObjectsDo: [ :each | 
			((each respondsTo: #releaseCachedState) and: [ (each isKindOf: RubAbstractTextArea) not ])
				ifTrue: [ each releaseCachedState ] ].
	
	self removeEmptyCategories.
	self removeEmptyPackages.
	Smalltalk organization sortCategories.
	Smalltalk garbageCollect.
	Smalltalk cleanOutUndeclared. 
	Smalltalk fixObsoleteReferences.
	Smalltalk cleanUp: true except: #() confirming: false.	
	Author reset
]

{ #category : #cleaning }
ImageCleaner >> cleanUpMethods [
	"Make sure that all methods in use are restarted"
	"self new cleanUpMethods"

	WeakArray restartFinalizationProcess.
	WorldState allInstancesDo: [ :ws | ws convertAlarms; cleanStepList; resetWorldMenu].
	WorldState allInstancesDo: [ :ws | ws instVarNamed: 'menuBuilder' put: nil ].
	ProcessBrowser initialize.
	Delay restartTimerEventLoop.
]

{ #category : #cleaning }
ImageCleaner >> cleanUpProcesses [

    (Process allInstances
               reject: [ :p |
                       {(InputEventFetcher default fetcherProcess).
                       (Processor activeProcess).
                       (WeakArray runningFinalizationProcess).
                      (Processor backgroundProcess).
                       (SmalltalkImage current lowSpaceWatcherProcess).
                       (UIManager default uiProcess).
                       (Delay schedulingProcess)} includes: p ])
               do: [ :p |
                       p suspend; terminate ]

]

{ #category : #cleaning }
ImageCleaner >> examplePackages [
	^RPackageOrganizer default packageNames select: [ :each | each endsWith: 'Examples' ]
]

{ #category : #cleaning }
ImageCleaner >> helpPackages [
	^RPackageOrganizer default packageNames select: [ :each | each endsWith: '-Help' ]
]

{ #category : #cleaning }
ImageCleaner >> packagesForCleanUpInProduction [
	"A list of packages who will be unloaded when going to production.
	 WARNING, ORDER IS IMPORTANT"
	
	^#(
	"Manifest & Critics Browser"
	'Manifest-Core'
	'Manifest-CriticBrowser'
	'Manifest-Tests'
	'Manifest-Resources-Tests'
	"Metacello"
	'Metacello-ToolBox'
	'Metacello-MC'
	'Metacello-Platform'
	'Metacello-Core'
	'Metacello-Base'
	'Metacello-ProfStef'
	'Metacello-Tutorial'
	'Metacello-TestsCore'
	'Metacello-TestsMC'
	'Metacello-TestsMCCore'
	'Metacello-TestsMCResources'
	'Metacello-TestsPharo20MC'
	'Metacello-TestsPlatform'
	'Metacello-TestsTutorial'

	)

]

{ #category : #cleaning }
ImageCleaner >> removeEmptyCategories [

	"Remove empty categories, which are not in MC packages, because MC does
	not do this (this script does not make packages dirty)"
	Smalltalk organization removeEmptyCategories.
	Smalltalk
		allClassesAndTraitsDo: [ :class |
			[ :each |
				each
					removeEmptyCategories;
					sortCategories ]
						value: class organization;
						value: class class organization ].
]

{ #category : #cleaning }
ImageCleaner >> removeEmptyPackages [
	| empty |
	empty := RPackageOrganizer default packages select: #isEmpty.
	empty do: #unregister.
]

{ #category : #cleaning }
ImageCleaner >> testPackages [
	^(RPackageOrganizer default  packageNames select: [ :each | each endsWith: 'Tests' ]) copyWithout: 'ReleaseTests'
]
