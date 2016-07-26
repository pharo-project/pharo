visual properties
passiveDisabledNotOverDownFillStyle: aFillStyle
	"Set the passive, disabled, notOver, down fill style."

	self stateMap atPath: #(passive disabled notOver down) put: aFillStyle.
	self changed