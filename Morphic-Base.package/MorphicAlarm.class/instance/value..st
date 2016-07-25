evaluating
value: anArgument
	| nArgs |
	numArgs ifNil:[numArgs := selector numArgs].
	nArgs := arguments ifNil:[0] ifNotNil:[arguments size].
	nArgs = numArgs ifTrue:[
		"Ignore extra argument"
		^self value].
	^arguments ifNil: [receiver perform: selector with: anArgument]
		ifNotNil: [receiver perform: selector withArguments: (arguments copyWith: anArgument)]