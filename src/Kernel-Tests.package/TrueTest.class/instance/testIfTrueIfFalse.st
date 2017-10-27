tests - controlling
testIfTrueIfFalse

	self assert: (true ifTrue: ['trueAlternativeBlock'] 
                      ifFalse: ['falseAlternativeBlock']) = 'trueAlternativeBlock'. 