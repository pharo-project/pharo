tests-arguments
testExtractTwoArgsWithAndWithoutMinus
	| args extract keys |
	args := #('ArgWithoutMinus' '-ArgWithMinus').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	keys := extract keys sort.
	self assert: extract isDictionary.
	self assert: extract size equals: 2.
	self assert: (keys at: 1) equals: #*.
	self assert: (keys at: 2) equals: #ArgWithMinus.
	self assert: (extract at: #*) equals: (Array with: 'ArgWithoutMinus').
	self assert: (extract at: #ArgWithMinus) isEmpty