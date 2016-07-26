accessing - bounds
innerBounds

	^ innerBounds ifNil: [ self updateCache. innerBounds ]