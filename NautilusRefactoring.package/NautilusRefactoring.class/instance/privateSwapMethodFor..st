private-method
privateSwapMethodFor: aCollection
	^ aCollection
		collect: [ :aMethod | RBSwapMethodRefactoring model: environment swapMethod: aMethod selector in: aMethod methodClass ]