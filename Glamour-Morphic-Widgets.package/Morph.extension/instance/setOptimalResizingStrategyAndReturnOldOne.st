*glamour-morphic-widgets
setOptimalResizingStrategyAndReturnOldOne
	| tmpHResizing tmpVResizing tmpSubmorphResizing |
	tmpHResizing := self hResizing.
	tmpVResizing := self vResizing.
	self setOptimalResizing.
	tmpSubmorphResizing := self submorphs collect: [ :aMorph | aMorph setOptimalResizingStrategyAndReturnOldOne ].
	^ {tmpHResizing.
	tmpVResizing.
	tmpSubmorphResizing}