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
	#instVars : [
		'literalTable'
	],
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

{ #category : #'instance creation' }
ImageCleaner class >> shareLiterals [
	<script>
	self new shareLiterals
]

{ #category : #accessing }
ImageCleaner class >> testPackages [

	<script: 'self testPackages inspect'>
	"All test packages to wipe out, except ReleeaseTests package."
	^ RPackageOrganizer default testPackageNames copyWithout:
		  'ReleaseTests'
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

	Smalltalk
		garbageCollect;
		cleanOutUndeclared;
		fixObsoleteReferences;
		cleanUp: true except: #() confirming: false.

	FreeTypeFontProvider current prepareForRelease.

	HashedCollection rehashAll.
	self shareLiterals.
	Author reset
]

{ #category : #cleaning }
ImageCleaner >> cleanUpMethods [
	"Make sure that all methods in use are restarted"
	<script: 'self new cleanUpMethods'>
	WeakArray restartFinalizationProcess.
	ToolRegistry allSubInstances do: [:each | each resetAnnouncer].

	WorldState
		allInstancesDo: [ :ws | ws convertAlarms; cleanStepList].

	ProcessBrowser initialize.
	Delay restartTimerEventLoop.
	"We are moving all the restart code to the classes"
	Smalltalk restartMethods
]

{ #category : #cleaning }
ImageCleaner >> cleanUpProcesses [
	(Process allInstances
		reject: [ :p |
			{
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

{ #category : #'literal sharing' }
ImageCleaner >> createLiteralTable [
	| arrays |
	literalTable := OCLiteralSet new.
	self literalsDo: [ :literal :method | literalTable add: literal.
		"we add all the contentents of the arrays recursively, too"
		literal isArray ifTrue: [
			literal recursiveDo: [ :each |
				(each isSymbol not and: [ each isImmediateObject not and: [each isNotNil]])
					ifTrue: [ literalTable add: each ] ] ] ].

	"we need to go over all array and unify. As we have all flattend arrays in the literalTable, we need to only have to iterate it"
	arrays := literalTable select: [ :each | each isArray ].
	arrays do: [ :array |
			array beWritableObject.
			array copy do: [ :arrayValue |
				(arrayValue  isSymbol not and:  [ arrayValue isImmediateObject not ]) ifTrue:
				[array
					at: (array identityIndexOf: arrayValue)
					put: (literalTable like: arrayValue)]].
			array beReadOnlyObject ]
]

{ #category : #'literal sharing' }
ImageCleaner >> detroyLiteralTable [
	"in case someone keeps in instance of ImageCleaner, make sure the table gets GCed"
	literalTable := nil
]

{ #category : #cleaning }
ImageCleaner >> examplePackages [

	^ self class examplePackages
]

{ #category : #cleaning }
ImageCleaner >> helpPackages [

	^ self class helpPackages
]

{ #category : #'literal sharing' }
ImageCleaner >> literalsDo: aBlock [

	CompiledCode allSubInstancesDo: [ :cm |
		cm literals do: [ :literal |
			"Symbols are already shared, we can skip them here"
			(literal isLiteral and: [literal isSymbol not and: [literal isImmediateObject not and: [literal isNotNil]]])
				ifTrue: [ aBlock value: literal value: cm ] ] ]
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

	Smalltalk organization removeEmptyPackages.
	Smalltalk allClassesAndTraitsDo: [ :class |
		class organization removeEmptyProtocols.
		class class organization removeEmptyProtocols ]
]

{ #category : #cleaning }
ImageCleaner >> removeEmptyPackages [
	| empty |
	empty := RPackageOrganizer default packages select: #isEmpty.
	empty do: #unregister
]

{ #category : #'literal sharing' }
ImageCleaner >> shareLiterals [
	"We go over all methods and replace all the literals that are equal by one copy"
	self createLiteralTable.
	self literalsDo: [ :literal :cm |
			cm literalAt: (cm indexOfLiteral: literal) put: (literalTable like: literal)  ].
	self detroyLiteralTable
]

{ #category : #cleaning }
ImageCleaner >> testPackages [

	^ self class testPackages
]
