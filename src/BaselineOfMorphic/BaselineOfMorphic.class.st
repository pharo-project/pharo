"
This baseline is used to generate Pharo images from the local clone of pharo-project/pharo repository. 

Depends on:
- BaselineOfMorphicCore

Load type: 
- atomic

Installs:
- Fuel
- Text and fonts support
- Morphic widgets
- Polymorph
- settings
- some other packages
"
Class {
	#name : 'BaselineOfMorphic',
	#superclass : 'BaselineOf',
	#instVars : [
		'initializersEnabled'
	],
	#classVars : [
		'Initialized'
	],
	#category : 'BaselineOfMorphic',
	#package : 'BaselineOfMorphic'
}

{ #category : 'baseline' }
BaselineOfMorphic >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
		spec preLoadDoIt: #preload:package:.
		spec postLoadDoIt: #postload:package:.
			
		spec package: 'System-Identification'.
		spec package: 'System-Settings-Core' with: [ spec requires: #( 'System-Identification' ) ].
		spec package: 'StartupPreferences'. "This should move out of Morphic but currently there is a cyclic dependency with System-Settings-Core..."

		spec package: 'System-Localization'.

		spec package: 'Graphics-Shapes'.
		spec package: 'Growl'.

		spec package: 'Native-Browser'.

		spec package: 'Regex-Core'.

		spec package: 'Morphic-Base'.
		spec package: 'Morphic-Widgets-Basic'.
		spec package: 'Morphic-Widgets-ColorPicker'.
		spec package: 'Morphic-Widgets-Extra'.
		spec package: 'Morphic-Widgets-Pluggable'.
		spec package: 'Morphic-Widgets-PolyTabs'.
		spec package: 'Morphic-Widgets-Scrolling'.
		spec package: 'Morphic-Widgets-Tabs'.
		spec package: 'Morphic-Widgets-Taskbar'.
		spec package: 'Morphic-Widgets-MissionControl'.
		
		spec package: 'Morphic-Widgets-Menubar'.
		spec package: 'Morphic-Widgets-Tree'.
		spec package: 'Morphic-Widgets-Windows'.
		spec package: 'Morphic-Widgets-FastTable'.

		spec package: 'Morphic-Widgets-Taskbar-Tests'.
		spec package: 'Morphic-Widgets-MissionControl-Tests'.

		"Several packages depend on Rubric as a basic Morphic component"
		spec package: 'Rubric'.

		spec baseline: 'NewValueHolder' with: [
			spec
				repository: repository;
				loads: 'core' ].

		spec package: 'DarkBlueTheme'.

		spec package: 'Settings-Graphics'.
		spec package: 'Settings-Polymorph'.
		spec package: 'Settings-System'.

		spec package: 'System-History'.
		spec package: 'PragmaCollector'.
		spec package: 'System-Settings-Browser'
			with: [ spec requires: #( 'PragmaCollector' ) ].
		"the following package is listed here to be after Setting core 
		however conceptually it could be placed somewhere else."
		spec package: 'Formatter-Settings'.

		spec package: 'Text-Diff'.
		spec package: 'Text-Edition'.
		spec package: 'Text-Scanning'.

		"Tool-Base extends and uses morphic, so it depends on it"
		spec package: 'Tool-Base' ]
]

{ #category : 'actions' }
BaselineOfMorphic >> cleanUpAfterMorphicInitialization [

	FreeTypeCache clearCurrent.
	World cleanseOtherworldlySteppers.
	MCFileBasedRepository flushAllCaches
]

{ #category : 'actions' }
BaselineOfMorphic >> postload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	SystemNotification signal: 'Starting postload action'.

	Stdio stdout
		nextPutAll: Smalltalk ui displayString;
		lf.

	#( #ProgressBarMorph #GlobalIdentifier #HaloMorph
	   #CharacterScanner #ImageMorph
	   #Locale #MenuItemMorph
	   #MenuMorph #RxMatcher #RxParser #RxsPredicate #SHA1
	   #SystemProgressMorph #SystemWindow #TextContainer
	   #TextDiffBuilder #ThumbnailMorph #TransferMorph #UITheme )
		do: [ :each | (self classNamed: each) initialize ].

	MorphicUIManager new spawnNewProcess.
	UIManager default: MorphicUIManager new.
	MorphicUIManager uiProcess resume.

	ErrorHandler default: ErrorHandler forCurrentSystemConfiguration.

	self cleanUpAfterMorphicInitialization.

	GraphicFontSettings setFontsToStyleNamed: #small.

	"Install the right UIManager"
	UIManager default: UIManager forCurrentSystemConfiguration.
	Stdio stdout
		nextPutAll: 'UIManager changed:';
		nextPutAll: UIManager default class name;
		lf.

	MenubarMorph initialize.

	PolymorphSystemSettings showDesktopLogo: false.
	PolymorphSystemSettings showDesktopLogo: true.
	PolymorphSystemSettings desktopColor: Color white.

	#( #BalloonMorph #IconicButtonMorph #ScrollBarMorph
	   #SystemSettingsPersistence #TabMorph #TaskbarMorph PharoOptionsHandler) do: [ :each |
		(self classNamed: each) initialize ].

	MCMethodDefinition initializersEnabled: initializersEnabled.

	Initialized := true
]

{ #category : 'actions' }
BaselineOfMorphic >> preload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].
	
	initializersEnabled := MCMethodDefinition initializersEnabled.
	MCMethodDefinition initializersEnabled: false.
]

{ #category : 'baseline' }
BaselineOfMorphic >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
]
