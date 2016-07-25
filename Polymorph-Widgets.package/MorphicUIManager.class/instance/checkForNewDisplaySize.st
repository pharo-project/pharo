ui requests
checkForNewDisplaySize

	"Check whether the screen size has changed and if so take appropriate actions"

	Display extent = DisplayScreen actualScreenSize ifTrue: [^ Display].
	DisplayScreen startUp.
	World restoreMorphicDisplay.
