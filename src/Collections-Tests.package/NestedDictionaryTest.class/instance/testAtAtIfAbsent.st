tests
testAtAtIfAbsent
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	d at: #top at: #below1 put: 2.
	d at: #fop at: #below1 put: 1.
	self assert: (d at: #top at: #below1) equals: 2.
	self assert: (d at: #fop at: #below666 ifAbsent: [ 666 ]) equals: 666.
	self assert: (d at: #fop666 at: #below1 ifAbsent: [ 666 ]) equals: 666.
	"when the first key is not found do not execute the second one."
	self assert: (d at: #fop666 at: [ 1 / 0 ] ifAbsent: [ 666 ]) equals: 666