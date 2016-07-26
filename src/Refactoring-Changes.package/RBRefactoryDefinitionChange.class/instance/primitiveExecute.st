private
primitiveExecute
	definedClass := self definitionClass compiler
		source: self definition;
		requestor: self controller;
		logged: true;
		evaluate