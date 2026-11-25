"
I set up the Morphic environment once all of Pharo is loaded: I initialize the classes whose class-side initialization needs the rest of Morphic (fonts, theme, UI manager), start the Morphic UI process and install its UI manager and error handler.

The bootstrap calls me as a stage of its own, after loading Pharo, and saves the image:

	pharo Pharo.image eval --save ""MorphicInitialization initializeEnvironment""

Loading packages never starts the UI by itself.
"
Class {
	#name : 'MorphicInitialization',
	#superclass : 'Object',
	#category : 'Morphic-Initialization',
	#package : 'Morphic-Initialization'
}

{ #category : 'initialization' }
MorphicInitialization class >> cleanUp [

	FreeTypeCache clearCurrent.
	World cleanseOtherworldlySteppers.
	MCFileBasedRepository flushAllCaches
]

{ #category : 'initialization' }
MorphicInitialization class >> createWorld [
	"Loading Morphic never creates a world: this is where Pharo's desktop world is born"

	DesktopWorldMorph installNewWorld
]

{ #category : 'initialization' }
MorphicInitialization class >> initializeEnvironment [
	"Set up Morphic after loading Pharo, and start its UI. Run once per image, before saving it.
	The bars are sized with the fonts"

	self
		loadBitmapFonts;
		createWorld;
		resetKeymaps;
		startUIProcess;
		cleanUp;
		useDefaultTheme;
		initializeFonts.
	PolymorphSystemSettings desktopLogo: (self initializeWorld: self currentWorld).
	self initializePreferences
]

{ #category : 'initialization' }
MorphicInitialization class >> initializeFonts [

	SourceCodeFonts setSourceCodeFonts: 10.
	FreeTypeSystemSettings loadFt2Library: true.
	GraphicFontSettings setFontsToStyleNamed: #small.
	"The balloon font is taken from the fonts just set"
	BalloonMorph balloonFont: StandardFonts defaultFont
]

{ #category : 'initialization' }
MorphicInitialization class >> initializePreferences [
	"Where windows and notifications open. The desktop color comes from the theme"

	RealEstateAgent usedStrategy: #cascadeFor:initialExtent:world:.
	GrowlMorph position: #bottomLeft.
	ShortcutReminder enabled: true
]

{ #category : 'initialization' }
MorphicInitialization class >> initializeWorld: aWorld [
	"Open the bars of aWorld, then put the desktop logo below its menubar. Answer the logo, nil when it is not shown"

	MenubarMorph openIn: aWorld.
	aWorld showWorldTaskbar: TaskbarMorph showTaskbar.
	^ PolymorphSystemSettings newDesktopLogo ifNotNil: [ :logo |
		PolymorphSystemSettings placeDesktopLogo: logo in: aWorld.
		logo ]
]

{ #category : 'fonts' }
MorphicInitialization class >> loadBitmapDejaVuSans [

	| dejaVu |

	dejaVu := self loadBitmapFontFamily: 'Bitmap DejaVu Sans' sizes: #(7 9 12 14 18 24).

	TextConstants TextSharedInformation at: #'DefaultTextStyle' put: dejaVu.
	TextConstants TextSharedInformation at: #'Bitmap DejaVu Sans' put: dejaVu.
	TextConstants TextSharedInformation at: #'DefaultMultiStyle' put: dejaVu.
	TextConstants TextSharedInformation at: #'DefaultFixedTextStyle' put: dejaVu.
]

{ #category : 'fonts' }
MorphicInitialization class >> loadBitmapFontFamily: familyName sizes: pointSizes [
	| fonts fileName directoryReference |
	directoryReference := (OSEnvironment current
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
MorphicInitialization class >> loadBitmapFonts [
	"Load the bitmap fonts of the repository's resources and make DejaVu Sans the default text style"

	self loadBitmapDejaVuSans.
	self loadBitmapSourcePro
]

{ #category : 'fonts' }
MorphicInitialization class >> loadBitmapSourcePro [

	| sansPro codePro |

	sansPro := self loadBitmapFontFamily: 'Bitmap Source Sans Pro' sizes: #(9 10 11 12 14 18 24).
	TextConstants TextSharedInformation at: #'Bitmap Source Sans Pro' put: sansPro.

	codePro := self loadBitmapFontFamily: 'Bitmap Source Code Pro' sizes: #(9 10 11 12 14 18 24).
	TextConstants TextSharedInformation at: #'Bitmap Source Code Pro' put: codePro.
]

{ #category : 'initialization' }
MorphicInitialization class >> resetKeymaps [
	"Start the new world with a fresh keymap repository, and let morphs handle their shortcuts with it"

	KMRepository reset.
	Morph shortcutsHandler: KMShortcutHandler new
]

{ #category : 'initialization' }
MorphicInitialization class >> startUIProcess [

	MorphicUIManager new spawnNewProcess.
	UIManager default: MorphicUIManager new.
	MorphicUIManager uiProcess resume.
	ErrorHandler default: ErrorHandler forCurrentSystemConfiguration.
	"Back to the UI manager this session runs with (non interactive from the command line)"
	UIManager default: UIManager forCurrentSystemConfiguration
]

{ #category : 'initialization' }
MorphicInitialization class >> useDefaultTheme [

	PolymorphSystemSettings defaultTheme beCurrent
]
