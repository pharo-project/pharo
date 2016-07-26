private
closeDataSocket
	self dataSocket
		ifNotNil: [
			self dataSocket closeAndDestroy.
			self dataSocket: nil]
