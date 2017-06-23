initialization
initializeWidgets
	remoteUrlLabel := self newLabel label: 'Remote URL'.
	remoteUrl := self newTextInput ghostText: 'git@github.com:user/project.git'; autoAccept: true.
	self focusOrder add: remoteUrl.
	super initializeWidgets.

	

