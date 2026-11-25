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
	BalloonMorph initialize
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
