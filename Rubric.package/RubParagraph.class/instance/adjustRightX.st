geometry updating
adjustRightX
	| shrink |
	shrink := self container right - self maxRightX.
	self lines do: [ :line | line paddingWidth: (line paddingWidth - shrink)].
	self container: (self container withRight: self maxRightX)