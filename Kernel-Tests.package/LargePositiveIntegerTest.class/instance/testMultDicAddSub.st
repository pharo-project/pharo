tests
testMultDicAddSub
	"self run: #testMultDicAddSub"

	| n f f1 |	
	n := 100.
	f := 100 factorial.
	f1 := f*(n+1).
	n timesRepeat: [f1 := f1 - f].
	self assert: (f1 = f). 

	n timesRepeat: [f1 := f1 + f].
	self assert: (f1 // f = (n+1)). 
	self assert: (f1 negated = (Number readFrom: '-' , f1 printString)).