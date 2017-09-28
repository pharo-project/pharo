public
propagate: aModification to: aTarget
	origin := aModification.
	target := aTarget.
	self classLayout: (target classLayout 
		reshapeFrom: origin oldLayout slotScope
		to: origin newLayout).