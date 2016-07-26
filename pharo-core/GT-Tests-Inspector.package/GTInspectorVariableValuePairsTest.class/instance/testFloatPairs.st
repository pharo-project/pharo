float
testFloatPairs
	| pairs |
	
	pairs := 4.2 gtInspectorVariableValuePairs.
	self assert: pairs size = 2.
	
	self assert: (pairs first = ('1' -> 1074842828)).
	self assert: (pairs second = ('2' -> 3435973837)).