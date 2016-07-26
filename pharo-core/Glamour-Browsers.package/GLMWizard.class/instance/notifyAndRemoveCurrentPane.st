building pane
notifyAndRemoveCurrentPane

	|oldPane   tmpPanes|
	tmpPanes := self stepsAndPanes at: self currentStep ifAbsentPut: [OrderedCollection new].
	oldPane :=  tmpPanes last.
	tmpPanes removeLast.
	self announce: (GLMPaneRemoved new
							pane: oldPane;
							browser: self;
							yourself ) 
	