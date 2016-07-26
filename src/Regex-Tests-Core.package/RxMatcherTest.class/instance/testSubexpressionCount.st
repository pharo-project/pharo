testing-protocol
testSubexpressionCount
	| matcher |
	#(('a' 1) ('a(b)' 2) ('a(b(c))' 3) ('(a)(b)' 3) ('(a(b))*' 3)) do: [ :pair |
		matcher := self matcherClass forString: pair first.
		matcher supportsSubexpressions 
			ifTrue: [ self assert: matcher subexpressionCount = pair last ] ]