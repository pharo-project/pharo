brick-tab-label-actions
beNormal

	self removeAllBricks.
	self addBrickBack: self normalLabel.
	self themer tabLabelThemer normalStyleFor: self.
	self changed