current 
current: aSoundSystem

	Current ifNotNil: [Current deactivate].
	Current := aSoundSystem.
	Current activate.