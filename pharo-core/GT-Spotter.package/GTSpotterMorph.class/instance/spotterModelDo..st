private
spotterModelDo: aBlock
	self spotterModel handleExceptionsDuring: [ 
		aBlock cull: self spotterModel.
		self handleProcessorKeyBindings ]