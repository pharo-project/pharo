*Polymorph-Widgets
replaceBoxes
	"Rebuild the various boxes."
	
	labelArea removeAllMorphs.
	self setLabelWidgetAllowance.
	self theme configureWindowLabelAreaFor: self.
	self setFramesForLabelArea.
	self isActive ifFalse: [labelArea passivate]