file support
askForFileNameToSave: suggestedFileName
	"Ask user for the name of the file to save workspace content to. Use the given file name as the initial answer"

	^ UIManager default
			request: 'Enter file name to save the workspace to' translated 
			initialAnswer: suggestedFileName