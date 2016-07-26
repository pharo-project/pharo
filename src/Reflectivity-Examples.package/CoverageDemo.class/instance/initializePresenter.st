initialization
initializePresenter

	installMetalinkButton action: [self installMetalink].
	runCodeButton action: [ self runCoverageCode ].
	
	"on close"
	self whenWindowChanged: [  
		self window whenClosedDo: [ self removeMetalink ]
	].