*Polymorph-Widgets
overrideExtent: newExtent
	"If autoFit is on then override to false for the duration of the extent call."
	
	self isAutoFit
		ifTrue: [self
				setProperty: #autoFitContents toValue: false;
				extent: newExtent;
				setProperty: #autoFitContents toValue: true]