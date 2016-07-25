visual properties
activeDisabledNotOverUpFillStyle: aFillStyle
	"Set the active, disabled, notOver, up fill style."

	self stateMap atPath: #(active disabled notOver up) put: aFillStyle.
	self changed