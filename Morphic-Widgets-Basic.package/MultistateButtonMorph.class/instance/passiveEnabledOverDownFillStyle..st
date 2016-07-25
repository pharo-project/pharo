visual properties
passiveEnabledOverDownFillStyle: aFillStyle
	"Set the passive, enabled, over, down fill style."

	self stateMap atPath: #(passive enabled over down) put: aFillStyle.
	self changed