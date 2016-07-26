tests - evaluating
testValueWithExitContinue

	| val last |	
	val := 0. 

	1 to: 10 do: [ :i |
		[ :continue |
			i = 4 ifTrue: [continue value].
			val := val + 1.
			last := i
		] valueWithExit.
	].

	self assert: val = 9.
	self assert: last = 10.	