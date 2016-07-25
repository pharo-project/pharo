emitting code
emitReturn: aBuilder resultTempVar: resultVar context: aContext
	^ aBuilder 
		addTemp: #tmpResult;
		"keep invoke result into the tmpResult var"
		storeTemp: resultVar;
		popTop;
		"return := self objectClass basicNew"
		pushLiteralVariable: self objectClass binding;
		send: #basicNew;
		storeTemp: #tmpResult;
		popTop;
		"return instVarAt: (index of argName) put: result"
		pushTemp: #tmpResult;
		pushLiteral: (self objectClass 
			instVarIndexFor: self instanceVariableName 
			ifAbsent: [ self error: 'No handle instVar' ]);
		pushTemp: resultVar;
		send: #instVarAt:put:;
		popTop;
		pushTemp: #tmpResult;
		returnTop	