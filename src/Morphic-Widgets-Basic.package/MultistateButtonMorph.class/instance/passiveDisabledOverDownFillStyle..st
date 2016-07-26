visual properties
passiveDisabledOverDownFillStyle: aFillStyle
	"Set the passive, disabled, over, down fill style."

	self stateMap atPath: #(passive disabled over down) put: aFillStyle.
	self changed