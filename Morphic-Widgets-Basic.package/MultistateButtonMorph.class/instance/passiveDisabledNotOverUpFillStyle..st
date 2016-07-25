visual properties
passiveDisabledNotOverUpFillStyle: aFillStyle
	"Set the passive, disabled, notOver, up fill style."

	self stateMap atPath: #(passive disabled notOver up) put: aFillStyle.
	self changed