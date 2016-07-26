tests - evaluating
testValueWithExitBreak

	| val |	

	[ :break |
	    1 to: 10 do: [ :i |
			val := i.
			i = 4 ifTrue: [break value].
		] 
	] valueWithExit.

	self assert: val = 4.