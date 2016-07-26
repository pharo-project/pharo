testing
canAccept: aRBRefactoring 

	^ (aRBRefactoring class whichClassIncludesSelector: #storeOn:) ~= Object