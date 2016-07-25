testing
testParser
	"Just confirm it will parse"
	
	(Parser new parse: (MethodNode sourceCodeAt: #primitiveErrorVariableName) class: MethodNode) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111 error: ''foo''> self primitiveFailed' class: Object) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111> self primitiveFailed' class: Object) primitiveErrorVariableName.
	(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) generate.
