building pane
notifyAndAddNewPane: aGLMPane

	(self stepsAndPanes at: self currentStep ifAbsentPut: [OrderedCollection new])
		addLast: aGLMPane.  
	self announce: (GLMPaneAdded new
							pane: aGLMPane;
							browser: self;
							yourself ) 
	