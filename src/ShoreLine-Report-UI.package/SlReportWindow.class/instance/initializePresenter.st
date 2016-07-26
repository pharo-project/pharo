initialization
initializePresenter
	
	sendButton action: [ self submitAndClose ].
	cancel action: [ self close ].
	help action: [ self openHelp ].