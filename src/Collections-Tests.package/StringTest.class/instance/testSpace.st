tests - instance creation
testSpace
	"self debug: #testSpace"
	
	string := String new.
	self assert: string size = 0. "instead of #isEmpty to be consistent with the following test"
	
	string := String space.
	self assert: string size = 1.
	self assert: string = ' '