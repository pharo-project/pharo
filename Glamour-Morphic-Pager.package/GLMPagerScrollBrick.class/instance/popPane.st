api
popPane
	
	self band doNotLayoutDuring: [ :b | b removeBrick: b lastSubbrick ].
	self doLayoutForce