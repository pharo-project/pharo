initialization
initialize
	"Initialize the state of the receiver."

	super initialize.
	self
		rubberBandCells: false;
		listDirection: #topToBottom;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		wrapCentering: #center;
		cellPositioning: #center.
	enabled := true.
	self initializeLabelMorph.	"this is a safe guard for enabled: among others."
	askBeforeChanging := false.
	triggerOnMouseDown := false.
	showSelectionFeedback := false.
	arguments := #().
	self
		layoutInset: (self theme buttonLabelInsetFor: self);
		extent: 20 @ 15;
		lastState: false;
		cornerStyle: (self theme buttonCornerStyleIn: nil).
	self theme buttonBorderStyleFor: self.
	self clipSubmorphs: true