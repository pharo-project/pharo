testing
sourceMatchesBytecodeAt: selector
	"Answers true if the source code at the selector compiles to the bytecode at the selector, and false
	otherwise. Implemented to detect an error where Monticello did not recompile sources when the class
	shape changed"

	"This code was copied from #recompile:from:, with few changes. Several methods would benefit from a
	method which turned a selector and class into a CompiledMethod, without  installing it into the 
	methodDictionary"

	| method newMethod |

	method := self compiledMethodAt: selector.
	newMethod := self compiler
			source: (self sourceCodeAt: selector);
			class: self;
			failBlock: [^ false];
			compiledMethodTrailer: method trailer;
			compile.   "Assume OK after proceed from SyntaxError"
	selector == newMethod selector ifFalse: [self error: 'selector changed!!'].
	^ newMethod = method