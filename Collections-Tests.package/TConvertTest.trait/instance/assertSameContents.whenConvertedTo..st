tests - converting
assertSameContents: aCollection whenConvertedTo: aClass 
	| result |
	result := self assertNonDuplicatedContents: aCollection whenConvertedTo: aClass.
	self assert: result size = aCollection size