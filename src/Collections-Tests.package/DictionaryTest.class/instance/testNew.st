tests - new
testNew
	| d |
	d := self classToBeTested new: 10.
	self assert: d size = 0.
	
	"Why 14? Mysterious"
	"self assert: d capacity = 14"