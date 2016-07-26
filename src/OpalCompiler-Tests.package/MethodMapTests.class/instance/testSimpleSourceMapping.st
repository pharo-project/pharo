testing - source mapping
testSimpleSourceMapping
	| method range highlight |

	method := Object>>('ha', 'lt') asSymbol.

	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: 23.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'now'.


	