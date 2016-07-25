initialization
initializePresenter
	"Used to specify the subwidgets, and/or to bind them together"
	"By default, do not do anything"
	
	extentHolder whenChangedDo: [:ex | 
		self widget ifNotNil: [:widget |
			(widget respondsTo: #extent:)
				ifTrue: [ widget extent: ex ]]].