dropping/grabbing
acceptDroppingMorph: aMorph event: evt

	self model
		acceptDroppingMorph: aMorph
		event: evt
		inMorph: self.
	self resetPotentialDropMorph.
	evt hand releaseMouseFocus: self.
	Cursor normal show.
