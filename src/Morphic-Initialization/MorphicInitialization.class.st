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
MorphicInitialization class >> initializeEnvironment [
	"Set up Morphic after loading Pharo, and start its UI. Run once per image, before saving it."

	self
		startUIProcess;
		cleanUp;
		initializeFontsAndDesktop;
		installRootEventHandlers;
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
MorphicInitialization class >> installRootEventHandlers [
	"Meta gestures on the world open halos and morph menus; Alt+Tab switches between windows"

	World
		addRootEventHandler: TaskListRootEventHandler new;
		addRootEventHandler: HaloRootEventHandler new
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
