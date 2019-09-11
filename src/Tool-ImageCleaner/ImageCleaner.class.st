"
Used for cleaning the image.

You can use one of my 2 methods: 
- CleanUpForRelease
	reinitialize the fonts
	reset metacello registry
	remove Empty Categories
	remove Empty Packages
	sort Categories
	remove undeclared globals
	remove obsolete references
	Launch the smalltalk cleanup
		run clean up on all the instances of the classes that implement cleanUp
- CleanUpForProduction
	All the previous actions +
	remove the background logo
	remove the tests
	remove the tools
	remove the versionner
"
Class {
	#name : #ImageCleaner,
	#superclass : #Object,
	#category : #'Tool-ImageCleaner'
}

{ #category : #'instance creation' }
ImageCleaner class >> cleanUpForProduction [
	<script>
	self new cleanUpForProduction
]

{ #category : #'instance creation' }
ImageCleaner class >> cleanUpForRelease [
	<script>
	self new cleanUpForRelease
]

{ #category : #accessing }
ImageCleaner class >> examplePackages [
	<script: 'self examplePackages inspect'>
	
	^RPackageOrganizer default packageNames select: [ :each | each endsWith: '-Examples' ]
]

{ #category : #accessing }
ImageCleaner class >> helpPackages [
	<script: 'self helpPackages inspect'>
	
	^RPackageOrganizer default packageNames select: [ :each | each endsWith: '-Help' ]
]

{ #category : #accessing }
ImageCleaner class >> testPackages [
	<script: 'self testPackages inspect'>
	
	^ (RPackageOrganizer default packageNames
		select: [ :each | (each endsWith: '-Tests') or: [ each includesSubstring: '-Tests-' ] ])
		copyWithout: 'ReleaseTests'
]

{ #category : #api }
ImageCleaner >> cleanUpForProduction [
	<script: 'self new cleanUpForProduction'>

	|unloading|	
	PolymorphSystemSettings showDesktopLogo: false.
	
	unloading := [ :each | (MCPackage named: each) unload ].	
	self packagesForCleanUpInProduction do: unloading.
	self class helpPackages do: unloading.
	self class testPackages do: unloading.
	self class examplePackages do: unloading.
	
	self cleanUpForRelease
]

{ #category : #api }
ImageCleaner >> cleanUpForRelease [
	<script: 'self new cleanUpForRelease'>

	Author fullName: 'MrCleaner'.
	self cleanUpMethods.
	self class environment at: #MetacelloProjectRegistration ifPresent: [ :class | class resetRegistry ].
	SystemNavigation new
		allObjectsDo: [ :each | 
			((each respondsTo: #releaseCachedState) and: [ (each isKindOf: RubAbstractTextArea) not ])
				ifTrue: [ each releaseCachedState ] ].
	
	self 
		removeEmptyCategories;
		removeEmptyPackages.
		
	Smalltalk organization sortCategories.

	Smalltalk 
		garbageCollect;
		cleanOutUndeclared; 
		fixObsoleteReferences;
		cleanUp: true except: #() confirming: false.	
	HashedCollection rehashAll.		
	Author reset
]

{ #category : #cleaning }
ImageCleaner >> cleanUpMethods [
	"Make sure that all methods in use are restarted"
	<script: 'self new cleanUpMethods'>
	WeakArray restartFinalizationProcess.
	
	WorldState 
		allInstancesDo: [ :ws | ws convertAlarms; cleanStepList; resetWorldMenu];
		allInstancesDo: [ :ws | ws instVarNamed: 'menuBuilder' put: nil ].
		
	ProcessBrowser initialize.
	Delay restartTimerEventLoop.
]

{ #category : #cleaning }
ImageCleaner >> cleanUpProcesses [
	(Process allInstances
		reject: [ :p | 
			{InputEventFetcher default fetcherProcess.
			Processor activeProcess.
			WeakArray runningFinalizationProcess.
			Processor backgroundProcess.
			SmalltalkImage current lowSpaceWatcherProcess.
			UIManager default uiProcess.
			Delay schedulingProcess} includes: p ])
		do: [ :p | 
			p
				suspend;
				terminate ]
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
	
	^#('MonticelloMocks')

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
	^ (RPackageOrganizer default packageNames
		select: [ :each | (each endsWith: '-Tests') or: [ each includesSubstring: '-Tests-' ] ])
		copyWithout: 'ReleaseTests'
]
