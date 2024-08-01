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

		spec
			baseline: 'MorphicCore' with: [ spec repository: repository ];
			baseline: 'MenuRegistration' with: [ spec repository: repository ].

		spec baseline: 'Fuel' with: [
			spec
				repository: repository;
				loads: #( 'Core' ) ].

		spec package: 'System-Localization'.

		spec baseline: 'FreeType' with: [
			spec
				repository: repository;
				loads: 'ui' ].
		spec package: 'EmbeddedFreeType' with: [ spec requires: 'FreeType' ].
		spec package: 'FileSystem-Zip'.
		spec package: 'Fonts-Infrastructure'.

		spec package: 'Graphics-Files'.
		spec package: 'Graphics-Fonts'.
		spec package: 'Graphics-Shapes'.
		spec package: 'Growl'.

		spec baseline: 'Keymapping' with: [
			spec
				repository: repository;
				loads: 'core' ].

		spec package: 'Transcript-Core'.

		spec package: 'Native-Browser'.

		spec package: 'Regex-Core'.

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
		spec package: 'Morphic-Widgets-Menubar'.
		spec package: 'Morphic-Widgets-Tree'.
		spec package: 'Morphic-Widgets-Windows'.
		spec package: 'Morphic-Widgets-FastTable'.

		spec package: 'Morphic-Widgets-Taskbar-Tests'.

		"Several packages depend on Rubric as a basic Morphic component"
		spec package: 'Rubric'.

		spec baseline: 'NewValueHolder' with: [
			spec
				repository: repository;
				loads: 'core' ].

		spec package: 'Polymorph-Widgets'.

		spec package: 'Images-Animated'.

		spec package: 'Settings-Graphics'.
		spec package: 'Settings-Polymorph'.
		spec package: 'Settings-System'.

		spec package: 'System-History'.
		spec package: 'System-Installers'.
		spec package: 'System-Settings-Core'.
		spec package: 'System-Settings-Browser'.
		"the following package is listed here to be after Setting core 
		however conceptually it could be placed somewhere else."
		spec package: 'Formatter-Settings'.
		spec package: 'System-Sound'.

		spec package: 'Text-Diff'.
		spec package: 'Text-Edition'.
		spec package: 'Text-Scanning'.

		spec package: 'Morphic-Widgets-TickList'.

		spec package: 'System-Identification'.

		"Tool-Base extends and uses morphic, so it depends on it"
		spec package: 'Tool-Base' ]
]

{ #category : 'actions' }
BaselineOfMorphic >> cleanUpAfterMorphicInitialization [

	MCDataStream initialize.
	GradientFillStyle initPixelRampCache.
	FreeTypeCache clearCurrent.
	ImageMorph
		writeClassVariableNamed: #DefaultForm
		value: (Form extent: 1 @ 1 depth: 1).
	World cleanseOtherworldlySteppers.
	MCFileBasedRepository flushAllCaches.
	MCMethodDefinition shutDown.
	ExternalDropHandler resetRegisteredHandlers.
	ScrollBarMorph initializeImagesCache
]

{ #category : 'fonts' }
BaselineOfMorphic >> exportBitmapDejaVuSans [

	| familyName styleNames pointSizes family font fileName fontCopy style |

	familyName := 'Bitmap DejaVu Sans'.
	styleNames := #('Regular' 'Bold' 'Italic' 'Bold Italic').
	pointSizes := #(7 9 12 14 18 24).

	family := LogicalFontManager current allFamilies detect: [ :f | 
		f familyName = 	familyName ] ifNone: [  ].

	pointSizes do: [ :pointSize | 
		styleNames do: [ :styleName |

			style := family members detect: [ :m | m styleName = styleName ].
			font := style asLogicalFontOfPointSize: pointSize.
			font := font realFont.

			fileName := font familyName, ' ', pointSize asString, ' ', styleName.

			font glyphs writePNGFileNamed: fileName, '.png'.

			fontCopy := font copy.
			fontCopy instVarNamed: #derivativeFonts put: nil.
			fontCopy setGlyphs: fileName.

			STON put: fontCopy onStream: (fileName, '.ston') asFileReference writeStream ]].
]

{ #category : 'fonts' }
BaselineOfMorphic >> loadBitmapDejaVuSans [

	| dejaVu |

	dejaVu := self loadBitmapFontFamily: 'Bitmap DejaVu Sans' sizes: #(7 9 12 14 18 24).

	TextConstants TextSharedInformation at: #'DefaultTextStyle' put: dejaVu.
	TextConstants TextSharedInformation at: #'Bitmap DejaVu Sans' put: dejaVu.
	TextConstants TextSharedInformation at: #'DefaultMultiStyle' put: dejaVu.
	TextConstants TextSharedInformation at: #'DefaultFixedTextStyle' put: dejaVu.
]

{ #category : 'fonts' }
BaselineOfMorphic >> loadBitmapFontFamily: familyName sizes: pointSizes [
	| fonts fileName directoryReference |
	directoryReference := (Smalltalk os environment
		                       at: 'BOOTSTRAP_REPOSITORY'
		                       ifAbsent: [ FileSystem workingDirectory ])
		                      asFileReference / 'resources' / 'fonts'.
	fonts := pointSizes collect: [ :pointSize | 
		         | regular bold italic boldItalic |
		         fileName := familyName , ' ' , pointSize asString.
		         regular := STON fromStream:
			                    (directoryReference
			                     / (fileName , ' Regular.ston')) readStream.
		         regular setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Regular.png'))
					          fullName).
		         bold := STON fromStream:
			                 (directoryReference / (fileName , ' Bold.ston'))
				                 readStream.
		         bold setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Bold.png')) fullName).
		         italic := STON fromStream:
			                   (directoryReference
			                    / (fileName , ' Italic.ston')) readStream.
		         italic setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Italic.png'))
					          fullName).
		         boldItalic := STON fromStream:
			                       (directoryReference
			                        / (fileName , ' Bold Italic.ston'))
				                       readStream.
		         boldItalic setGlyphs: (PNGReadWriter formFromFileNamed:
				          (directoryReference / (fileName , ' Bold Italic.png'))
					          fullName).
		         regular instVarNamed: #derivativeFonts put: { 
				         bold.
				         italic.
				         boldItalic } , (Array new: 29).
		         bold instVarNamed: #derivativeFonts put: (Array new: 32).
		         italic instVarNamed: #derivativeFonts put: (Array new: 32).
		         boldItalic
			         instVarNamed: #derivativeFonts
			         put: (Array new: 32).
		         regular ].
	^ TextStyle fontArray: fonts
]

{ #category : 'fonts' }
BaselineOfMorphic >> loadBitmapSourcePro [

	| sansPro codePro |

	sansPro := self loadBitmapFontFamily: 'Bitmap Source Sans Pro' sizes: #(9 10 11 12 14 18 24).
	TextConstants TextSharedInformation at: #'Bitmap Source Sans Pro' put: sansPro.

	codePro := self loadBitmapFontFamily: 'Bitmap Source Code Pro' sizes: #(9 10 11 12 14 18 24).
	TextConstants TextSharedInformation at: #'Bitmap Source Code Pro' put: codePro.
]

{ #category : 'actions' }
BaselineOfMorphic >> postload: loader package: packageSpec [

	| array |
	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	SystemNotification signal: 'Starting postload action'.

	TextConstants initialize.

	"Initialize Fuel"
	FLLargeIdentityHashedCollection initialize.

	Stdio stdout
		nextPutAll: 'starting...';
		lf.

	Stdio stdout
		nextPutAll: Smalltalk ui asString;
		lf.

	PNGReadWriter initialize.
	self loadBitmapDejaVuSans.
	self loadBitmapSourcePro.

	(Smalltalk globals at: #TextStyle) initialize.

	Stdio stdout
		nextPutAll: 'bitmap fonts loaded' asString;
		lf.

	#( #ProgressBarMorph #BalloonEngineConstants #BalloonEngine
	   #CornerRounder #DigitalSignatureAlgorithm
	   #FT2Types #FT2FFILibrary #FreeTypeCacheConstants
	   #FreeTypeCache #FreeTypeSettings #FreeTypeSubPixelAntiAliasedGlyphRenderer
	   #FT2Constants #GIFReadWriter #GlobalIdentifier #HaloMorph
	   #CharacterScanner #ImageMorph #JPEGHuffmanTable #JPEGReadStream
	   #Locale #MczInstaller #MD5NonPrimitive
	   #MenuItemMorph #MenuMorph #RxMatcher #RxParser #RxsPredicate
	   #SHA1 #ShortIntegerArray #SystemProgressMorph #SystemWindow
	   #TestCase #TextContainer #TextDiffBuilder #ThumbnailMorph
	   #TransferMorph #UITheme #ZnByteEncoder #ZnConstants #ZnMimeType
	   #ZnNetworkingUtils #ZnServer #ZnSingleThreadedServer ) do: [ :each |
		(Smalltalk globals at: each) initialize ].

	BalloonMorph setBalloonColorTo: Color yellow.

	UIManager default terminateUIProcess.
	MorphicUIManager new spawnNewProcess.

	UIManager default: MorphicUIManager new.
	MorphicUIManager uiProcess resume.

	ErrorHandler defaultErrorHandler: UIManager default.

	PharoLightTheme beCurrent.
	ScalingCanvas initialize.
	self cleanUpAfterMorphicInitialization.

	GraphicFontSettings setFontsToStyleNamed: #small.

	"Install the right UIManager"
	UIManager default: UIManager forCurrentSystemConfiguration.
	ErrorHandler defaultErrorHandler: UIManager default.
	Stdio stdout
		nextPutAll: 'UIManager changed:';
		nextPutAll: UIManager default class name;
		lf.

	MenubarMorph initialize.

	PolymorphSystemSettings showDesktopLogo: false.
	PolymorphSystemSettings showDesktopLogo: true.
	PolymorphSystemSettings desktopColor: Color white.

	#( #AbstractFont #BalloonMorph #Clipboard #EmbeddedFreeTypeFontInstaller
	   #FLCompiledMethodCluster #FLLargeIdentityHashedCollection
	   #FreeTypeFontProvider #GradientFillStyle #IconicButtonMorph
	   #KMLog #LogicalFont #LucidaGrandeRegular #OSEnvironment
	   #ScrollBarMorph #StrikeFont #SystemSettingsPersistence
	   #TabMorph #TaskbarMorph #TextAction #TextConstants #TextStyle )
		do: [ :each | (Smalltalk globals at: each) initialize ].

	LogicalFontManager current addFontProvider:
		FreeTypeFontProvider current.

	array := Smalltalk specialObjectsArray copy.
	array at: 5 put: Bitmap.
	Smalltalk specialObjectsArray becomeForward: array.

	MCMethodDefinition initializersEnabled: initializersEnabled.

	Initialized := true
]

{ #category : 'actions' }
BaselineOfMorphic >> preload: loader package: packageSpec [

	"Ignore pre and post loads if already executed"
	Initialized = true ifTrue: [ ^ self ].

	SystemNotification signal: 'Starting preload action'.

	"Declare global variables"
	Smalltalk globals at: #World put: nil.
	Smalltalk globals at: #ActiveWorld put: nil.
	
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
