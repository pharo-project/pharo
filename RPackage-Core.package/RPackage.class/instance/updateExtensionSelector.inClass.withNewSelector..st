updating
updateExtensionSelector: oldSelector inClass: aClass withNewSelector: newSelector

	aClass isMeta
		ifTrue: [
			(metaclassExtensionSelectors at: aClass instanceSide name asSymbol) remove: oldSelector. 
			(metaclassExtensionSelectors at: aClass instanceSide name asSymbol) add: newSelector. 
			]
		ifFalse: [
			(classExtensionSelectors  at: aClass name asSymbol) remove: oldSelector. 
			(classExtensionSelectors  at: aClass name asSymbol) add: newSelector. 
			]