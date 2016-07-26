menu
openOnCurrentWorkingConfiguration

	CriticWorkingConfiguration exists
		ifTrue: [  ResetWindow new openWithSpec  ]
		ifFalse: [ SelectPackageBrowser open]