accessing
threshold: aTreshold
	(aTreshold ~= threshold and: [aTreshold between: 0 and: 1000])
		ifTrue: [
			threshold := aTreshold.
			self updateList].
	self changed: #threshold