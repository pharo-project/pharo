accessing
ensureReplacePresentationsStrategy
	self transmissionStrategy class = GLMReplacePresentationsStrategy ifFalse: [
		self transmissionStrategy: (GLMReplacePresentationsStrategy of: self) ]