tests - controlling
testIfFalseIfTrue

	self assert: (false ifFalse: ['falseAlternativeBlock'] 
                      ifTrue: ['trueAlternativeBlock']) = 'falseAlternativeBlock'. 