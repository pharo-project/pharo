tests
testBlockFilter
	| fromBlockFilter |
	
	fromBlockFilter := [ :ctx | ctx isNotNil ] asFilter.
	self assert: (fromBlockFilter shouldDisplay: context)