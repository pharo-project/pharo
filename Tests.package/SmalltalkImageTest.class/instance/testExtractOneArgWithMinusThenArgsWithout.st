tests-arguments
testExtractOneArgWithMinusThenArgsWithout
	| args extract |
	args := #('-ArgWithMinus' 'ArgWithoutMinus1' 'ArgWithoutMinus2').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract isDictionary.
	self assert: extract size equals: 1.
	self assert: (extract keys at: 1) equals: #ArgWithMinus.
	self assert: (extract at: #ArgWithMinus) equals: #('ArgWithoutMinus1' 'ArgWithoutMinus2')