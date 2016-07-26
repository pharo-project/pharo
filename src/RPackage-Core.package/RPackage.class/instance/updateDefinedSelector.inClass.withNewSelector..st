updating
updateDefinedSelector: oldSelector inClass: aClass withNewSelector: newSelector

	aClass isMeta
		ifTrue: [
			(metaclassDefinedSelectors at: aClass instanceSide name asSymbol) remove: oldSelector. 
			(metaclassDefinedSelectors at: aClass instanceSide name asSymbol) add: newSelector. ]
		ifFalse: [
			(classDefinedSelectors  at: aClass name asSymbol) remove: oldSelector. 
			(classDefinedSelectors  at: aClass name asSymbol) add: newSelector. ]