visual properties
passiveEnabledNotOverUpFillStyle: aFillStyle
	"Set the passive, enabled, notOver, up fill style."

	self stateMap atPath: #(passive enabled notOver up) put: aFillStyle.
	self changed