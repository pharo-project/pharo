private
moveBy: aPoint
	self lines do: [ :line | line moveBy: aPoint ].
	theContainer := theContainer translateBy: aPoint.
	maxRightX := maxRightX + aPoint x