visual properties
passiveDisabledOverUpFillStyle: aFillStyle
	"Set the passive, disabled, over, up fill style."

	self stateMap atPath: #(passive disabled over up) put: aFillStyle.
	self changed