visual properties
activeEnabledOverUpFillStyle: aFillStyle
	"Set the active, enabled, over, up fill style."

	self stateMap atPath: #(active enabled over up) put: aFillStyle.
	self changed