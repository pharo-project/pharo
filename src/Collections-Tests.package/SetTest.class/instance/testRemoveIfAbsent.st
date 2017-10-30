tests - some
testRemoveIfAbsent
	"self debug: #testRemoveIfAbsent"
	
	self assert: (full remove: 8 ifAbsent: [true]).
	self assert: (full remove: 4 ifAbsent: [false]) isNumber.
	
	
	