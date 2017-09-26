tests
testObjects
	"self debug: #testObjects"

	self assert: 10 isSelfEvaluating.
	self assert: $a isSelfEvaluating.
	self assert: 3.14157 isSelfEvaluating.
	self assert: #(1 2 3) isSelfEvaluating.
	self assert: #abc isSelfEvaluating.
	self assert: 'abc' isSelfEvaluating.

	self assert: Object isSelfEvaluating.
	self assert: Object new isSelfEvaluating not.
	
	self assert: {Float infinity . Float nan. Float infinity negated} isSelfEvaluating.

	self assert: (Array with: 10) isSelfEvaluating.
	self assert: (Array with: Object new) isSelfEvaluating not.

	self assert: true isSelfEvaluating.
	self assert: false isSelfEvaluating.

	self assert: nil isSelfEvaluating.

	self assert: (1 to: 10) isSelfEvaluating.
	self assert: (1->2) isSelfEvaluating.
	self assert: Color red isSelfEvaluating.
