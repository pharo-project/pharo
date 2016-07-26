brick-layout-properties
vShrinkWrap

	self height: [
		self vPadding + 
		(self icon isNil & self hoverIcon isNil
			ifTrue: [ 0 ]
			ifFalse: [
				self icon
					ifNil: [ self hoverIcon height ]
					ifNotNil: [ 
						self hoverIcon
							ifNil: [ self icon height ]
							ifNotNil: [ self icon height max: self hoverIcon height ] ] ]) ]