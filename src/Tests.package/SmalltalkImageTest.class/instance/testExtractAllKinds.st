tests-arguments
testExtractAllKinds
	| args extract keys |
	args := #('ArgWithoutMinus1' '-ArgWithMinus1' 'ArgWithoutMinus2' 'ArgWithoutMinus3' '-ArgWithMinus2').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	keys := extract keys sort.
	self assert: extract isDictionary.
	self assert: extract size = 3.
	self assert: (keys at: 1) = #*.
	self assert: (keys at: 2) = #ArgWithMinus1.
	self assert: (keys at: 3) = #ArgWithMinus2.
	self assert: (extract at: #*) = (Array with: 'ArgWithoutMinus1').
	self assert: (extract at: #ArgWithMinus1) = #('ArgWithoutMinus2' 'ArgWithoutMinus3').
	self assert: (extract at: #ArgWithMinus2) isEmpty