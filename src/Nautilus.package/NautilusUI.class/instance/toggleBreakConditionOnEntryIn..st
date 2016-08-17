menus behavior
toggleBreakConditionOnEntryIn: aMethod
	"Install or uninstall a halt-on-entry breakpoint"

	aMethod ifNil: [ ^ self ].
	aMethod hasBreakpoint ifFalse: [ 
		(UIManager default request: 'Break on what condition?')	
		ifNotNil: [ :condition | Breakpoint new 
			node: aMethod ast;
			condition: (aMethod class compiler evaluate: condition);
			install]
		]