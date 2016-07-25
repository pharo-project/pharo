visual properties
activeEnabledNotOverDownFillStyle: aFillStyle
	"Set the active, enabled, notOver, down fill style."

	self stateMap atPath: #(active enabled notOver down) put: aFillStyle.
	self changed