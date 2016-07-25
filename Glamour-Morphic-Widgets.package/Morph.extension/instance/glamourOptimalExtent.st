*glamour-morphic-widgets
glamourOptimalExtent

	|tmpOldResizing tmpResult|
	tmpOldResizing := self setOptimalResizingStrategyAndReturnOldOne. 
	tmpResult := self minExtent.
	self returnToOldResizingStrategy:  tmpOldResizing.
	^ tmpResult 