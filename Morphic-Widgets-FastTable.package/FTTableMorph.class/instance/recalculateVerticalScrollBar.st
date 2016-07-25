private
recalculateVerticalScrollBar
	| interval delta pageDelta visibleRows numberOfRows |
	
	self hasDataSource ifFalse: [ ^ self ].

	self recalculateVerticalScrollBarVisibilityIfHidden: [ ^ self ].
	 
	visibleRows := self container calculateExactVisibleRows.	
	numberOfRows := self dataSource numberOfRows.
	interval := (visibleRows / numberOfRows) asFloat.
	delta := 1/numberOfRows.
	pageDelta := ((visibleRows-1) floor)*delta.
	self verticalScrollBar 
		scrollDelta: delta pageDelta: pageDelta;
		interval: interval