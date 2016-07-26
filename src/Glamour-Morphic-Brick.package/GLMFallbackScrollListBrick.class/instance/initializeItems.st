initialization
initializeItems
	
	self band layoutAfter: [
		1 to: self model count do: [ :index |
			self band addBrickBack: ((self model morphAt: index convert: nil) asBrick with:[:b |
				b brickApi layout vState isOnParent ifTrue: [ b height: 30 ]]) ] ].
	
	self band hasSubbricks ifFalse: [ self emptyBrick ifNotNil: [ :b | self band addBrickBack: b ] ].