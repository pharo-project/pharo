I am a meta-object that can be put on any AST node to count execution

To install a watchpoint in a node: 
	ExecutionCounter installOn: aNode
	
Class CounterIconStyler then renders an icon in the editor, mouse over it to see the current counter value.