private
computeChange
	super computeChange.
	layout computeChangesFrom: target classLayout in: self.
	slotShift := layout fieldSize - target classLayout fieldSize.
	self propagate.