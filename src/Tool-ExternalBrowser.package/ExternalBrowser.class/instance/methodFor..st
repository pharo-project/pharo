structure accessing
methodFor: aSelector

	^ (self classes selectedItem methodNamed: aSelector) sourceCode
			