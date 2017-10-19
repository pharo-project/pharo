tests - controlling
testIfFalseIfTrue

	self assert: (true ifFalse: ['falseAlternativeBlock'] 
                      ifTrue: ['trueAlternativeBlock']) = 'trueAlternativeBlock'. 