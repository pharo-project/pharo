tests - size capacity
testSize

	| size |
	self assert: self empty size = 0.
	size := 0.
	self sizeCollection do: [ :each | size := size + 1].
	
	self assert: self sizeCollection size = size.