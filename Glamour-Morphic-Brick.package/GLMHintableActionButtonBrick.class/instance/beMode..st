initialization
beMode: aMode

	self hoverAction: [ :aBrick :evt |
			self class asyncTaskUniqueInstance
				perform: [ self isInWorld ifTrue: [ self class openHint: self hintText title: self hintTitle type: aMode for: self ] ]
				delay: ((self isModifierPressed: evt) ifTrue: [ 0 ] ifFalse: [ self hintDelay ]) ]