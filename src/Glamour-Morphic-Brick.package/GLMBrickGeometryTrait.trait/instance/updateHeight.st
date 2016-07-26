private-geometry
updateHeight
	"no need to care about height, when it's logic is not customized"
	self layout isHeightCustom ifFalse: [ ^ self ].
	[ self bounds: (self brickBounds withHeight: self customHeight) ]
		on: Exception
		do: [ "just skip and do nothing" ]