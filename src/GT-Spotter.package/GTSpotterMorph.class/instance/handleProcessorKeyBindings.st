private
handleProcessorKeyBindings
	self processorKeyBindingsDo: [
		self spotterModel currentStep ifNotNil: [ 
			self spotterModel currentStep processors do: [ :processor | 
				processor keyBinding ifNotNil: [ 
					| keyCombination |
					keyCombination := processor keyBinding asKeyCombination.
					processorKeyBindings at: keyCombination put: processor.
					self kmDispatcher bindKeyCombination: keyCombination toAction: [ self onKeyProcessor: processor ] ] ] ] ]