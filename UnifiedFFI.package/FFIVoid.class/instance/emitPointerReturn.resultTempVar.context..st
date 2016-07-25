emitting code
emitPointerReturn: aBuilder resultTempVar: resultVar context: aContext
	^ aBuilder 
		addTemp: #tmpResult;
		"keep invoke result into the tmpResult var"
		storeTemp: #tmpResult;
		popTop;
		"return := self objectClass basicNew"
		pushLiteralVariable: self objectClass binding;
		send: #basicNew;
		storeTemp: resultVar;
		popTop;
		"return instVarAt: (index of argName) put: tmpResult"
		pushTemp: resultVar;
		pushLiteral: (self objectClass 
			instVarIndexFor: #handle 
			ifAbsent: [ self error: 'No handle instVar' ]);
		pushTemp: #tmpResult;
		send: #instVarAt:put:;
		pushTemp: resultVar;
		returnTop	