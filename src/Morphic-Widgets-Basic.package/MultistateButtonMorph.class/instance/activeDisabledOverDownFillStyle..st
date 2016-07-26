visual properties
activeDisabledOverDownFillStyle: aFillStyle
	"Set the active, disabled, over, down fill style."

	self stateMap atPath: #(active disabled over down) put: aFillStyle.
	self changed