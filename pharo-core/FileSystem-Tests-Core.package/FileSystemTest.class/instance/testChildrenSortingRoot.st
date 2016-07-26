tests
testChildrenSortingRoot
	| directory1 directory2 |
	"self skip."
	
	directory1 := Path * 'plonk1'.
	directory2 := Path * 'plonk2'.
	
	filesystem createDirectory: directory1.
	filesystem createDirectory: directory2.
	
	self markForCleanup: directory1.
	self markForCleanup: directory2.
	
	self assert: filesystem workingDirectory children sorted size = filesystem workingDirectory children size