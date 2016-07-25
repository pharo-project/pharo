helper
testMethod: cm against: string

	| symbolic |
	self assert: cm isCompiledMethod.
	symbolic := String streamContents: [:str | cm longPrintOn: str ].
	self assert: symbolic = string
	