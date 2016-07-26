tests - element accessing
testAtIfAbsent
	"self debug: #testAt"
	| absent |
	absent := false.
	self moreThan4Elements 
		at: self moreThan4Elements size + 1
		ifAbsent: [ absent := true ].
	self assert: absent = true.
	absent := false.
	self moreThan4Elements 
		at: self moreThan4Elements size
		ifAbsent: [ absent := true ].
	self assert: absent = false