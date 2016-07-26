menu
showMenuForPosition: aPoint
	| tuple |
	
	tuple := self container rowAndColumnIndexContainingPoint: aPoint.
	self 
		showMenuForRowIndex: tuple first
		columnIndex: tuple second