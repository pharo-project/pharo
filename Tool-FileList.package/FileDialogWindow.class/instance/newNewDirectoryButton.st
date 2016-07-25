instance creation
newNewDirectoryButton
	"Answer a new 'new directory' button."

	^ self
		newButtonFor: self
		getState: nil
		action: #newDirectory
		arguments: nil
		getEnabled: nil
		labelForm: (Smalltalk ui icons iconNamed: #smallOpenIcon)
		help:
			'Press to create a new directory within the current directory'
				translated