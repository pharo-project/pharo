tests-arguments
testExtractOneArgNoMinus
	| args extract |
	args := #('ArgNoMinus').
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract isDictionary.
	self assert: extract size equals: 1.
	self assert: (extract keys at: 1) equals: #*.
	self assert: (extract at: #*) equals: args