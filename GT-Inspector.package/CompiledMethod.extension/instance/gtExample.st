*GT-Inspector-Examples-Core
gtExample
	^ self gtExamples 
		ifNotEmpty: [ :examples | 
			" there should only be one example per compiled-method, otherwise something is really messed up "
			self assert: examples size = 1. 
			examples first ]
		ifEmpty: [ self error: self printString, ' does not define any gt-example' ]