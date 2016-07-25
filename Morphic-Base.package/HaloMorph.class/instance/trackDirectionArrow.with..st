private
trackDirectionArrow: anEvent with: shaft
	anEvent hand obtainHalo: self.
	shaft setVertices: {directionArrowAnchor. anEvent cursorPoint}.
	self layoutChanged