tests - instance creation
testNew
	"self debug: #testNew"
	| array |
	array := RunArray new.
	self assert: array size = 0.