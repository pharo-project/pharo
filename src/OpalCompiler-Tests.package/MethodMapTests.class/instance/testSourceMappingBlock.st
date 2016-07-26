testing - source mapping
testSourceMappingBlock
	| method range highlight |

	method := MethodMapExamples>>#exampleTempNamedCopying.

	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: 42.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'b'.

	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: 43.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'a := b'.
	
	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: 44.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals:'DebuggerMethodMapOpal'.
	
	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: 45.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals:'thisContext'.
	
	range := (DebuggerMethodMapOpal forMethod: (self compileMethod: method)) rangeForPC: 46.
	highlight := method sourceCode copyFrom:  range first to: range last.
	self assert: highlight equals: 'method'.