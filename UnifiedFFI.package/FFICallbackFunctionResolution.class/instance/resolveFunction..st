resolving
resolveFunction: anExternalFunction
	| callback |
	callback := self class 
		callbackNamed: anExternalFunction name
		ifAbsent: [ self error: 'Callback #', anExternalFunction name, ' not registered!' ].
	^ ExternalAddress fromAddress: callback thunk address