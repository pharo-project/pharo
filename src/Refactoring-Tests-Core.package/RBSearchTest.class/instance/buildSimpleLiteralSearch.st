accessing
buildSimpleLiteralSearch
	| search |
	search := RBParseTreeSearcher new.
	search 
		matchesAnyTreeOf: (Array with: (RBParser parseExpression: '#(''bugs'' ''possible bugs'' ''unnecessary code'' ''intention revealing'' ''miscellaneous'')'))
		do: 
			[ :aNode :answer | 
			answer
				remove: self currentSelector
					ifAbsent: [ self error: 'failed' ];
				yourself ].
	search answer: #(#protocolsToCheck ) asBag.
	(classSearches 
		at: RBBasicLintRuleTest class
		ifAbsentPut: [ Set new ]) add: search