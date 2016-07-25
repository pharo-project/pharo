utilties
runRegex: anArray
	"Run a clause anArray against a set of tests. Each clause is an array with a regex source string followed by sequence of 3-tuples. Each three-element group is one test to try against the regex, and includes: 1) test string; 2) expected result; 3) expected subexpression as an array of (index, substring), or nil."

	| source matcher |
	source := anArray first.
	matcher := self compileRegex: source.
	matcher isNil
		ifTrue: [
			(anArray at: 2) isNil
				ifFalse: [ self signalFailure: 'Compilation failed, should have succeeded: ' , source printString ] ]
		ifFalse: [
			(anArray at: 2) isNil
				ifTrue: [ self signalFailure: 'Compilation succeeded, should have failed: ' , source printString ]
				ifFalse: [
					2 to: anArray size by: 3 do: [ :index | 
						self 
							runMatcher: matcher
							with: (anArray at: index)
							expect: (anArray at: index + 1)
							withSubexpressions: (anArray at: index + 2) ] ] ]