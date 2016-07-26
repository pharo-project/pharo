tests
testKeyAtValueIfAbsentLocalyDefined
	" self debug: #testKeyAtValueIfAbsentLocalyDefined "
	self assert: (self class methodDict keyAtValue: (self class >> #testKeyAtValueIfAbsentLocalyDefined) ifAbsent: []) == #testKeyAtValueIfAbsentLocalyDefined.
	
	self assert: (self class methodDict keyAtValue: (Object >> #printOn:) ifAbsent: [#notFound]) == #notFound