accessing
spotterModel: aSpotter

	spotterModel := aSpotter.
	
	spotterModel announcer when: GTSpotterExitAnnouncement send: #closeSpotter to: self.
	spotterModel announcer when: GTSpotterShowPreview send: #onShowPreview to: self.
	spotterModel announcer when: GTSpotterHidePreview send: #onHidePreview to: self.
	
	self breadcrumbBrick spotterModel: spotterModel.
	self paneBrick spotterModel: spotterModel.
	
	spotterModel class isPreviewVisible ifTrue: [ spotterModel showPreview ].
	
	"expanding drop down pane"
	self spotterModelDo: [ 
		self spotterModel setText: '' from: nil.
		self spotterModel currentStep process: nil ]
	
	
	