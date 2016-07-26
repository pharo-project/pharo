accessing
onColor: colorWhenOn offColor: colorWhenOff
	"Set the fill colors to be used when this button is on/off."

	onColor := colorWhenOn.
	offColor := colorWhenOff.
	self update: #onOffColor.
	self update: getStateSelector.


