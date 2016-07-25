filtering
updateAccordingTo: aFilter

	(aFilter keepTreeNode: self) 
		ifFalse: [
			self changed: #updateAccordingTo: with: { aFilter } ]