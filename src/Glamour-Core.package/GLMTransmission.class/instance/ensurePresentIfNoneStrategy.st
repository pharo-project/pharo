accessing
ensurePresentIfNoneStrategy
	self transmissionStrategy class = GLMPresentIfNoneStrategy ifFalse: [
		self transmissionStrategy: (GLMPresentIfNoneStrategy of: self) ]