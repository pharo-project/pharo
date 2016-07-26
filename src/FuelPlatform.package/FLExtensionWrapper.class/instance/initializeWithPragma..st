initialize-release
initializeWithPragma: aPragma
	| argumentNamesAndValues |
	argumentNamesAndValues := self readArgumentsFrom: aPragma.
	self
		method: aPragma method copy;
		selector: (self
			readSelectorFrom: aPragma
			argumentNames: argumentNamesAndValues first
			argumentValues: argumentNamesAndValues second);
		isSource: (self
			readIsSourceFrom: aPragma
			argumentNames: argumentNamesAndValues first
			argumentValues: argumentNamesAndValues second);
		targetClass: (self
			readTargetClassFrom: aPragma
			argumentNames: argumentNamesAndValues first
			argumentValues: argumentNamesAndValues second)