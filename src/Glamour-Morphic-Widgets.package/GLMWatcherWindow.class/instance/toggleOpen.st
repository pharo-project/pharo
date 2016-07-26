open/close
toggleOpen 
	isOpen ifFalse: [ ^ self openInWorld ].
	self visible 
		ifTrue: [self hide] 
		ifFalse: [self show]