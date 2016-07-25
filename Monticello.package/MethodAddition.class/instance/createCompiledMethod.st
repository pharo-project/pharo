operations
createCompiledMethod
	compiledMethod := myClass compiler 
		source: text asString;
		requestor: requestor;
		category: category;
		failBlock: [ ^nil ];
		compile.
	selector := compiledMethod selector.
	self writeSourceToLog.
	priorMethodOrNil := myClass compiledMethodAt: selector ifAbsent: [ nil ].
	priorCategoryOrNil := myClass organization categoryOfElement: selector