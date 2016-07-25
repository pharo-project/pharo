tests - sequence isempty
testSequenceIfNotEmptyifEmpty

	self assert: (self nonEmpty ifEmpty: [false] ifNotEmpty: [:s | (self accessValuePutInOn: s) = self valuePutIn])