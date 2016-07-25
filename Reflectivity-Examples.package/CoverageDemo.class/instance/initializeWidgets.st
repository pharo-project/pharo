initialization
initializeWidgets

	installMetalinkButton := self newButton.
	runCodeButton := self newButton.
	codePanelMorph := ReflectivitySourceCodeAgroupation new asSpecAdapter.
	
	installMetalinkButton label: 'Install Metalink'.
	runCodeButton label: 'Run Example Code'.