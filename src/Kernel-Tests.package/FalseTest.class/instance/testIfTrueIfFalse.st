tests - controlling
testIfTrueIfFalse

	self assert: (false ifTrue: ['trueAlternativeBlock'] 
                      ifFalse: ['falseAlternativeBlock']) = 'falseAlternativeBlock'. 