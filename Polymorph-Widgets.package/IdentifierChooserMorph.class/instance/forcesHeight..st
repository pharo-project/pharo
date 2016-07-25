ui-building
forcesHeight: aHeight
	| sp |
	(sp := self scrollPane) minHeight: aHeight.
	sp height: sp minHeight.
 	self height: sp height. 