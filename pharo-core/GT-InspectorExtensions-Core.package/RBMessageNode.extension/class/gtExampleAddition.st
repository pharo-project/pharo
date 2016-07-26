*GT-InspectorExtensions-Core
gtExampleAddition
	<gtExample>
	^ RBMessageNode 
				receiver: (RBVariableNode named: 'self')
				selector: #+
				arguments: (Array with: (RBLiteralNode value: 0))