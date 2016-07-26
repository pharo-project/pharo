tests-arguments
testExtractOneArgWithMinus
	| args extract |
	args := #('-ArgWithMinus').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract isDictionary.
	self assert: extract size equals: 1.
	self assert: (extract keys at: 1) equals: #ArgWithMinus.
	self assert: (extract at: #ArgWithMinus) isEmpty