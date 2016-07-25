private
handleLineForSelector: selector withSeparatorBlock: aBlock
	(self isLineTooLong: selector)
		ifTrue: [ self newLine ]
		ifFalse: [ aBlock value ]