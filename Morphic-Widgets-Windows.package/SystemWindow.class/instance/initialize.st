initialization
initialize
	"Initialize a system window. Add label, stripes, etc., if desired"
	
	super initialize.
	labelString ifNil: [labelString := 'Untitled Window'].
	isCollapsed := false.
	paneMorphs := Array new.
	self layoutPolicy: ProportionalLayout new.
	self clipSubmorphs: true.
	self theme
		configureWindowBorderFor: self;
		configureWindowDropShadowFor: self.	
	self initializeLabelArea.

	self cellPositioning: #topLeft. "make the offsets easy to calculate!"
	self addGripsIfWanted.
	
	self extent: 300 @ 200.
	mustNotClose := false.
	updatablePanes := Array new