accessing
primitiveErrorVariableName
	"Answer the primitive error code temp name, or nil if none."
	(primitive isInteger and: [primitive > 0]) ifTrue:
		[properties pragmas do:
			[:pragma| | kwds ecIndex |
			((kwds := pragma keyword keywords) first = 'primitive:'
			and: [(ecIndex := kwds indexOf: 'error:') > 0]) ifTrue:
				[^pragma argumentAt: ecIndex]]].
	^nil

	"(Parser new parse: (MethodNode sourceCodeAt: #primitiveErrorVariableName) class: Parser) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: ''foo''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' error: foo module: ''bar''> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: ''foo'' module: ''bar'' error: foo> self primitiveFailed' class: Object) primitiveErrorVariableName"

	"(Parser new parse: 'foo <primitive: 111 error: foo> self primitiveFailed' class: Object) generate"