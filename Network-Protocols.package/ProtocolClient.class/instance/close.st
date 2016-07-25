actions
close
	self stream
		ifNotNil: [
			self stream close.
			stream := nil]