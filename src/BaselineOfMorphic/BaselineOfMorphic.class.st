"
This baseline is used to generate Pharo images from the local clone of pharo-core repository. 

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
	#name : #BaselineOfMorphic,
	#superclass : #BaselineOf,
	#instVars : [
		'initializersEnabled'
	],
	#category : #BaselineOfMorphic
}

{ #category : #baseline }
BaselineOfMorphic >> baseline: spec [ 
    
	<baseline>
        
	| repository | 
	
	repository := 'tonel://./pharo-core/src'.    
	spec for: #common do: [
		spec preLoadDoIt: #'preload:package:'.
		spec postLoadDoIt: #'postload:package:'.

		spec baseline: 'MorphicCore' with: [spec repository: repository].

		spec package: 'System-Localization'.
		spec package: 'Collections-DoubleLinkedList'.
		spec package: 'Text-Core'.
		spec package: 'System-Clipboard'.
		
		spec package: 'Balloon'.
		spec package: 'EmbeddedFreeType'.
		spec package: 'EmergencyEvaluator'.
		spec package: 'FileSystem-Zip'.
		spec package: 'FontInfrastructure'.
		spec package: 'FreeType'.
		spec package: 'Fuel'.
		spec package: 'FuelPlatform-Core'.
		spec package: 'FuelPlatform-Pharo-Core'.
		spec package: 'FuelPlatform-Pharo-06'.
	
		spec package: 'FuelCommandLineHandler'.
		spec package: 'FuelSystem-FileRegistry'.

		spec package: 'Fonts-Abstract'.

		spec package: 'Graphics-Files'.
		spec package: 'Graphics-Fonts'.
		spec package: 'Graphics-Shapes'.
		spec package: 'Growl'.

		spec package: 'MenuRegistration'.

		spec package: 'Keymapping-Core'.
		spec package: 'Keymapping-KeyCombinations'.
		spec package: 'Keymapping-Pragmas'.

		spec package: 'Morphic-Base'.
		spec package: 'Morphic-Examples'.
		spec package: 'Morphic-Widgets-Basic'.
		spec package: 'Morphic-Widgets-ColorPicker'.
		spec package: 'Morphic-Widgets-Extra'.
		spec package: 'Morphic-Widgets-List'.
		spec package: 'Morphic-Widgets-Pluggable'.
		spec package: 'Morphic-Widgets-PolyTabs'.
		spec package: 'Morphic-Widgets-Scrolling'.
		spec package: 'Morphic-Widgets-Tabs'.
		spec package: 'Morphic-Widgets-Taskbar'.
		spec package: 'Morphic-Widgets-Tree'.
		spec package: 'Morphic-Widgets-Windows'.
		spec package: 'Multilingual-OtherLanguages'.
		spec package: 'Multilingual-TextConverterOtherLanguages'.

		spec package: 'NewValueHolder'.

		spec package: 'Polymorph-Widgets'.

		spec package: 'Settings-Graphics'.
		spec package: 'Settings-Polymorph'.
		spec package: 'Settings-System'.

		spec package: 'System-Caching'.
		spec package: 'System-History'.
		spec package: 'System-Installers'.
		spec package: 'System-Settings'.
		spec package: 'System-Sound'.
		
		spec package: 'Text-Diff'.
		spec package: 'Text-Edition'.
		spec package: 'Text-Scanning'.

		spec package: 'Unicode-Initialization'.

		spec package: 'Morphic-Widgets-TickList'.
		
		spec package: 'System-Changes-FileServices'.

		spec package: 'StartupPreferences'.
		spec package: 'System-Identification'.
	].

]

{ #category : #actions }
BaselineOfMorphic >> cleanUpAfterMorphicInitialization [

	Author fullName: self class name.
	MCDataStream initialize.
	GradientFillStyle initPixelRampCache.
	FreeTypeCache clearCurrent.
	ImageMorph classPool at: #DefaultForm put: (Form extent: 1 @ 1 depth: 1).
	Behavior flushObsoleteSubclasses.
	World cleanseOtherworldlySteppers.
	Smalltalk organization removeEmptyCategories.
	MCFileBasedRepository flushAllCaches.
	MCMethodDefinition shutDown. 
	MCDefinition clearInstances.
	ExternalDropHandler resetRegisteredHandlers.
	Undeclared removeUnreferencedKeys.
	Smalltalk globals flushClassNameCache.
	ScrollBar initializeImagesCache.

	3 timesRepeat: [ 
		Smalltalk garbageCollect.
	 	Symbol compactSymbolTable ].
	 	HashedCollection rehashAll.
	  
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
	Smalltalk organization sortCategories.
	ChangeSet removeChangeSetsNamedSuchThat: [ :each | true ].
	ChangeSet resetCurrentToNewUnnamedChangeSet.
	Smalltalk garbageCollect.
	Author reset.
]

{ #category : #actions }
BaselineOfMorphic >> postload: loader package: packageSpec [

	| dejavu array |

	'Starting postload action' logCr.

	Smalltalk cleanOutUndeclared.
	Author fullName: self class name.

	TextConstants initialize.
		
	"Initialize Fuel"

	FLLargeIdentityHashedCollection initialize.
	FLPlatform classPool at: #Current put: FLPharo6Platform new.

	Stdio stdout nextPutAll: 'starting...'; lf.

	Stdio stdout nextPutAll: Smalltalk ui asString; lf.

	SparseLargeTable recompile. 
	
	dejavu := FLMaterializer materializeFromFileNamed: 'BitmapDejaVuSans.fuel'.

	TextConstants TextSharedInformation at: #'DefaultTextStyle' put: dejavu.
	TextConstants TextSharedInformation at: #'Bitmap DejaVu Sans' put: dejavu.
	TextConstants TextSharedInformation at: #'DefaultMultiStyle' put: dejavu.
	TextConstants TextSharedInformation at: #'DefaultFixedTextStyle' put: dejavu.

	#(TextStyle) do: [:each | (Smalltalk globals at: each) initialize. ].

	Stdio stdout nextPutAll: 'bitmap fonts loaded' asString; lf.

	#(ProgressBarMorph BalloonBezierSimulation BalloonEngineConstants BalloonEngine 
	CornerRounder  DigitalSignatureAlgorithm DummySoundSystem 
	FreeTypeCacheConstants FreeTypeCache FreeTypeSettings 
	FreeTypeSubPixelAntiAliasedGlyphRenderer FT2Constants FT2Handle GIFReadWriter
	HaloMorph CharacterScanner ImageMorph JPEGHuffmanTable
	JPEGReadStream JPEGReadWriter 
	MczInstaller MD5NonPrimitive MenuItemMorph MenuMorph  
	PNGReadWriter RxMatcher RxParser RxsPredicate
	SHA1 ShortIntegerArray SimpleEditor
	SystemProgressMorph SystemWindow TestCase TextContainer TextDiffBuilder
	ThemeIcons ThumbnailMorph TransferMorph UITheme ZnByteEncoder
	ZnConstants ZnMimeType ZnNetworkingUtils ZnServer ZnSingleThreadedServer
	) do: [:each | (Smalltalk globals at: each) initialize. ].

	BalloonMorph setBalloonColorTo: Color yellow.

	UIManager default terminateUIProcess.
	MorphicUIManager new spawnNewProcess.
	Display newDepth: 32.
	World displayWorldSafely.
	UIManager default: MorphicUIManager new.
	World displayWorldSafely.
	UIManager default uiProcess resume.
	Pharo3Theme beCurrent.
	(World windowsSatisfying: [:w | w model canDiscardEdits]) do: [:w | w delete].
	self cleanUpAfterMorphicInitialization.

	GraphicFontSettings setFontsToStyleNamed: #small.

	TextEditor initialize.
	SmalltalkEditor initialize.

	PolymorphSystemSettings showDesktopLogo: false.
	PolymorphSystemSettings showDesktopLogo: true.
	PolymorphSystemSettings desktopColor: Color white.

	Smalltalk cleanOutUndeclared.

	#(#AbstractFont #BalloonMorph #CP1253TextConverter #Clipboard #EditorFindReplaceDialogWindow #EmbeddedFreeTypeFontInstaller #FLCompiledMethodCluster #FLLargeIdentityHashedCollection #FLPlatform #FreeTypeFontProvider #GB2312 #GradientFillStyle #GreekEnvironment #ISO88597TextConverter #IconicButton #JISX0208 #JapaneseEnvironment #KOI8RTextConverter #KSX1001 #KoreanEnvironment #LogicalFont #LucidaGrandeRegular #OSEnvironment #RussianEnvironment #ScrollBar #SimplifiedChineseEnvironment "#SourceCodeProRegular #SourceSansProRegular" #StrikeFont #SystemSettingsPersistence #TabMorph #TaskbarMorph #Text #TextAction #TextConstants #TextEditor #TextStyle) do: [:each |
		 (Smalltalk globals at: each) initialize. ].

	array := Smalltalk specialObjectsArray copy.
	array at: 5 put: Bitmap.
	Smalltalk specialObjectsArray becomeForward: array.

	Author reset.
	MCMethodDefinition initializersEnabled: initializersEnabled.

	UIManager default restoreDisplay. 
]

{ #category : #actions }
BaselineOfMorphic >> preload: loader package: packageSpec [

	'Starting preload action' logCr.
	
	initializersEnabled := MCMethodDefinition initializersEnabled.
	MCMethodDefinition initializersEnabled: false.
	
	Author fullName: self class name.
	

]

{ #category : #baseline }
BaselineOfMorphic >> project [
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
	
]
