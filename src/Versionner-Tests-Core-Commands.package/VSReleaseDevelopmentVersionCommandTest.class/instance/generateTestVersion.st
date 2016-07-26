test data
generateTestVersion
	| ast |

	ast := (project configurationClass methodNamed: #version101:) ast.
	ast selector: #testVersion:.
	(ast pragmas at: 1) arguments at: 1 put: (RBLiteralValueNode value: '3.0').
	project configurationClass compile: ast formattedCode