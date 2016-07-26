scrolling
update
	isUpdating := true.
	self updateVisible.
	self doNotLayoutDuring:  [ |delta|
		delta := self bandTopOffset + self topOffset.
		self band doLayoutForce.
		self band translateY: delta.  ].
	self preventOverscrolling.
	self verticalScrollbar updatePosition.
	isUpdating := false.