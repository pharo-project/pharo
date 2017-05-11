initialization
initialize
	super initialize.
	self enable.

	self themer spotterThemer itemNormalStyleFor: self.

	self iconBrick: self newIconBrick.
	self labelBrick: self newLabelBrick.
	self actionbarBrick: self newActionbarBrick