accessing
commitWithProgress: aTransferProgress
	self withReturnHandlerDo: [
		self prim_commit
			writepack: self
			progress: aTransferProgress ]