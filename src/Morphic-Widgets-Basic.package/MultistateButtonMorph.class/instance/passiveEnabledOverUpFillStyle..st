visual properties
passiveEnabledOverUpFillStyle: aFillStyle
	"Set the passive, enabled, over, up fill style."

	self stateMap atPath: #(passive enabled over up) put: aFillStyle.
	self changed