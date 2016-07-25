recompilation
recompile: selector from: oldClass
	| newMethod |
	newMethod := oldClass compiler
		source: (self decompile: oldClass >> selector) formattedCode;
		class: oldClass;
		failBlock: [ ^ self ];
		compiledMethodTrailer: CompiledMethodTrailer empty;
		compile.	"Assume OK after proceed from SyntaxError"
	selector == newMethod selector
		ifFalse: [ self error: 'selector changed!' ].
	oldClass basicAddSelector: selector withMethod: newMethod