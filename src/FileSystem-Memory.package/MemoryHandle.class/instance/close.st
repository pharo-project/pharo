public
close
	self isOpen ifFalse: [ ^ self ].
	self truncate.
	entry := nil.