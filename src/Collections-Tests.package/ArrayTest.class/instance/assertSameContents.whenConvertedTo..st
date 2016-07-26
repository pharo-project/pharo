tests - converting
assertSameContents: aCollection whenConvertedTo: aClass 
	| result |
	result := aCollection perform: ('as' , aClass name) asSymbol.
	self assert: (result class includesBehavior: aClass).
	result do: 
		[ :each | 
		self assert: (aCollection occurrencesOf: each) = (result occurrencesOf: each) ].
	self assert: result size = aCollection size