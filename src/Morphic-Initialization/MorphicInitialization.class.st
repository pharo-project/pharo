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
MorphicInitialization class >> initializeClasses [
	"These class initializers depend on each other and on fonts and the theme, so run them again, in this order, once everything is loaded."

	#( #ProgressBarMorph #GlobalIdentifier #HaloMorph #CharacterScanner #ImageMorph #Locale #MenuItemMorph #MenuMorph #SHA1 #SystemProgressMorph
	   #SystemWindow #TextContainer #TextDiffBuilder #ThumbnailMorph #TransferMorph #UITheme ) do: [ :each |
		(self class environment at: each) initialize ]
]

{ #category : 'initialization' }
MorphicInitialization class >> initializeEnvironment [
	"Set up Morphic after loading Pharo, and start its UI. Run once per image, before saving it."

	self
		initializeClasses;
		startUIProcess;
		cleanUp;
		initializeFontsAndDesktop;
		openWorldBars
]

{ #category : 'initialization' }
MorphicInitialization class >> initializeFontsAndDesktop [

	GraphicFontSettings setFontsToStyleNamed: #small.
	PolymorphSystemSettings showDesktopLogo: false.
	PolymorphSystemSettings showDesktopLogo: true.
	PolymorphSystemSettings desktopColor: Color white.
	"The balloon font is taken from the fonts just set"
	BalloonMorph initialize
]

{ #category : 'initialization' }
MorphicInitialization class >> openWorldBars [
	"Open the menubar and the taskbar in the world, as their preferences say"

	MenubarMorph reset.
	TaskbarMorph showTaskbarPreferenceChanged
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
