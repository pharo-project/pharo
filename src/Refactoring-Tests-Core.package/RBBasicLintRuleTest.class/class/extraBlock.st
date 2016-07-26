unnecessary code
extraBlock
	^self
		createParseTreeRule: (#('value' 'value: `@value' 'value: `@value1 value: `@value2' 'value: `@value1 value: `value2 value: `@value3' 'valueWithArguments: `@values')
				collect: [:each | '[:`@params | | `@temps | `@.statements] ' , each])
		name: 'Block immediately evaluated'