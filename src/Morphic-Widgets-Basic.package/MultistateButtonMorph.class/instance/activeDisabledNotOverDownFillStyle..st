visual properties
activeDisabledNotOverDownFillStyle: aFillStyle
	"Set the active, disabled, notOver, down fill style."

	self stateMap atPath: #(active disabled notOver down) put: aFillStyle.
	self changed