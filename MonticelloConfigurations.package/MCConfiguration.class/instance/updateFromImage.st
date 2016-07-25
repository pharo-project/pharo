updating
updateFromImage
	self dependencies: (self dependencies collect: [:dep |
		dep package hasWorkingCopy
			ifTrue: [
				dep package workingCopy in: [:wc |
					MCVersionDependency package: wc package info: wc ancestors first]]
			ifFalse: [dep]]).
