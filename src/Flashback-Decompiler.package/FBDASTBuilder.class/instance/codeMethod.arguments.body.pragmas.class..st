building
codeMethod: selector arguments: args body: body pragmas: pragmas class: class
	^ (RBMethodNode selector: selector arguments: args body: body)
		pragmas: pragmas;
		methodClass: class;
		yourself