private
waitingFolderMCZFiles
	^ self class defaultMCWaitingFolder allFileNames
		reject: [:each | each =  '.DS_Store']