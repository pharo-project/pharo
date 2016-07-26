file support
askForFileNameToOpen
	"Ask user for file name to load content from"
	^ UIManager default
			chooseFullFileNameMatching: nil 
			label: 'Choose file to open in the workspace' translated