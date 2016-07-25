tests - accessing
testRunLengthAt
	"self debug: #testRunLengthAt"
	| array |
	array := #($a $b $b $c $c $c $d $d) as: RunArray.
	self assert: (array runLengthAt: 1) = 1.
	self assert: (array runLengthAt: 2) = 2.
	self assert: (array runLengthAt: 3) = 1.
	self assert: (array runLengthAt: 4) = 3.
	self assert: (array runLengthAt: 5) = 2.
	self assert: (array runLengthAt: 6) = 1.
	self assert: (array runLengthAt: 7) = 2.
	self assert: (array runLengthAt: 8) = 1.