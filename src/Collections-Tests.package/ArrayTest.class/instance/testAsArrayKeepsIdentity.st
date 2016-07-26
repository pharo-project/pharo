tests - converting
testAsArrayKeepsIdentity
	 
	|x y|
	x := #(1 2 3 4 5).
	y := x asArray.
	self assert: y identityHash = x identityHash