accessing
needsStep

	^ doStep ifNil: [ doStep := true ]