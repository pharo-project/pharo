menus behavior
toggleBreakpoint
	| savedSelectedMethodIndex savedVscroll |
	self selectedMethod ifNil: [ ^ self ].

	savedSelectedMethodIndex := methodWidget selectedMethodIndex.
	savedVscroll := methodWidget vScrollValue.
	self class toggleBreakpointIn: self selectedMethod.
	self resetMethodsList.
	
	methodWidget invalidateCacheEntryFor: self selectedMethod.
	methodWidget selectedMethodIndex: savedSelectedMethodIndex.
	savedVscroll := methodWidget vScrollValue: savedVscroll