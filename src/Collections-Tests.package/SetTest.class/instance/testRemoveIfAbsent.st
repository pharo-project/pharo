tests - some
testRemoveIfAbsent
	"self debug: #testRemoveIfAbsent"
	| e |
	e := self element.	
	self assert: (full remove: elementNotIn ifAbsent: [true]).
	self assert: (full remove: e ifAbsent: [false]) equals: e
	
	
	