visual properties
activeDisabledOverUpFillStyle: aFillStyle
	"Set the active, disabled, over, up fill style."

	self stateMap atPath: #(active disabled over up) put: aFillStyle.
	self changed