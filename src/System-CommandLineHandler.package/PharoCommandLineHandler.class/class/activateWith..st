instance creation
activateWith: aCommandLine	
	"Make sure that the PharoCommandLineHandler starts at the top of the stack in the main UI thread."
	UIManager default defer:  [ 
		super activateWith: aCommandLine ]