initialize
replaceBoxes
	"Rebuild the various boxes."
	
	labelArea removeAllMorphs.
	self setLabelWidgetAllowance.
	self theme configureWatcherWindowLabelAreaFor: self.
	self setFramesForLabelArea.
	self isActive ifFalse: [labelArea passivate]