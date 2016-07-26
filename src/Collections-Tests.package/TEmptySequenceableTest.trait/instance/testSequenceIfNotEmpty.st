tests - sequence isempty
testSequenceIfNotEmpty

	self assert: (self nonEmpty 
					ifNotEmpty: [:s | self accessValuePutInOn: s]) = self valuePutIn