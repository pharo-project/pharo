user interface
content
	^ (self domainValues
		detect: [:v | v realValue = self realValue]
		ifNone: [^ 'nil']) name