visual properties
activeEnabledNotOverUpFillStyle: aFillStyle
	"Set the active, enabled, notOver, up fill style."

	self stateMap atPath: #(active enabled notOver up) put: aFillStyle.
	self changed