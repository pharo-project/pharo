brick-layout-properties
hShrinkWrap

	self width: [
		self vPadding + 
		(self icon isNil & self hoverIcon isNil
			ifTrue: [ 0 ]
			ifFalse: [
				self icon
					ifNil: [ self hoverIcon width ]
					ifNotNil: [ 
						self hoverIcon
							ifNil: [ self icon width ]
							ifNotNil: [ self icon width max: self hoverIcon width ] ] ]) ]