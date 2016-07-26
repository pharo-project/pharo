visual properties
activeEnabledOverDownFillStyle: aFillStyle
	"Set the active, enabled, over, down fill style."

	self stateMap atPath: #(active enabled over down) put: aFillStyle.
	self changed