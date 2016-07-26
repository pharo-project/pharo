private-geometry
updateWidth
	"no need to care about width, when it's logic is not customized"
	self layout isWidthCustom ifFalse: [ ^ self ].
	[ self bounds: (self brickBounds withWidth: self customWidth) ]
		on: Exception
		do: [ "just skip and do nothing" ]