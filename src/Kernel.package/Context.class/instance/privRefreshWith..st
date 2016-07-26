initialize-release
privRefreshWith: aCompiledMethod 
	"Reinitialize the receiver as though it had been for a different method. 
	 Used by a Debugger when one of the methods to which it refers is 
	 recompiled."

	aCompiledMethod isCompiledMethod ifFalse:
		[self error: 'method can only be set to aCompiledMethod'].
	method := aCompiledMethod.
	[closureOrNil == nil] assert.
	"was: receiverMap := nil."
	self privRefresh