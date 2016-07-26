miscellaneous
fullBlocks

"skip this test in squeak."

"	| detector |
	detector := self new.
	detector name: 'Method with full blocks'.
	detector methodBlock: 
			[:context :result | 
			context compiledMethod withAllBlockMethodsDo: 
					[:method | 
					method needsHybridFrame
						ifTrue: [result addClass: context selectedClass selector: context selector]]].
	^detector"