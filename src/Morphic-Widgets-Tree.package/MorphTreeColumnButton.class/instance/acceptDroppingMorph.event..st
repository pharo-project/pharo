accessing
acceptDroppingMorph: aMorph event: evt
	self model container swapColumn: self model withColumn: aMorph passenger.
	evt hand releaseMouseFocus: self.
	self noMorePotentialDropTarget.
	self model container changed.
	Cursor normal show.
