system startup
startUp
	World ifNotNil: [:world | world restoreMorphicDisplay].
	self extraWorldList do: #restoreMorphicDisplay.