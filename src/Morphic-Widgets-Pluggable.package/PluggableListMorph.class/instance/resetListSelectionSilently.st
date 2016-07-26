selection
resetListSelectionSilently

	self resetListSelector ifNotNil: [:sel | self model perform: sel ]