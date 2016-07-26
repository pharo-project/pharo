tests-arguments
testExtractNil
	| args extract |
	args := nil.
	extract := SmalltalkImage current extractMinusParametersFrom: args.
	self assert: extract class equals: Dictionary.
	self assert: extract isEmpty