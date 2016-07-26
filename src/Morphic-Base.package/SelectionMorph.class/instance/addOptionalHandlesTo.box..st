halos and balloon help
addOptionalHandlesTo: aHalo box: box
	aHalo addHandleAt: box leftCenter color: Color blue icon: nil
		on: #mouseUp send: #addOrRemoveItems: to: self.