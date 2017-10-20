defaults
promptForDefaultChangeSetDirectoryIfNecessary
	"Check the Preference (if any), and prompt the
	user to change it if necessary.
	The default if the Preference is unset is the current
	directory. Answer the directory."
	"ChangeSet
	promptForDefaultChangeSetDirectoryIfNecessary"
	| choice directoryPath dir message |
	directoryPath := self defaultChangeSetDirectoryName.
	[dir := directoryPath asFileReference.
	dir exists] 
		whileFalse: [message := 'The preferred change set directory' , ' (''{1}'') ' , 'does not exist.' , '
' , 'Create it or use the default directory' , ' ({2})?' format: {directoryPath. FileSystem workingDirectory fullName}.
			choice := UIManager default
						chooseFrom: (#('Create directory' 'Use default directory and forget preference' 'Choose another directory' )
								collect: [:ea | ea ])
						message: message.
			choice = 1
				ifTrue: [dir ensureCreateDirectory ].
			choice = 3
				ifTrue: [dir := UIManager default chooseDirectory.
					directoryPath := dir
								ifNil: ['']
								ifNotNil: [dir fullName]]].
	self defaultChangeSetDirectory: directoryPath.
	^ dir