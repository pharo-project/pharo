visual properties
passiveEnabledNotOverDownFillStyle: aFillStyle
	"Set the passive, enabled, notOver, down fill style."

	self stateMap atPath: #(passive enabled notOver down) put: aFillStyle.
	self changed