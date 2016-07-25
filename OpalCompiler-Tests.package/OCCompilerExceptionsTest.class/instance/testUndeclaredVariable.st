tests
testUndeclaredVariable

	self
		compiling: 'griffle ^ goo' shouldRaise: OCUndeclaredVariableWarning  ;
		compiling: 'griffle ^ [ goo ] value' shouldRaise: OCUndeclaredVariableWarning