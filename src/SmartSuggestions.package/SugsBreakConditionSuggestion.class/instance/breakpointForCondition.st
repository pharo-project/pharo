breakpoints
breakpointForCondition
	(UIManager default request: 'Break on what condition?')	
		ifNotNil: [ :condition | Breakpoint new 
			node: context selectedNode;
			condition: (OpalCompiler evaluate: condition);
			install]