running
check: anAST forCritiquesDo: aCriticBlock

	anAST isMethod ifFalse: [ ^ self ].
	anAST pragmaNamed: #gtInspectorPresentationOrder: ifAbsent: [ ^ self ].
	
	^ (anAST sendNodes
			reject: [ :message |
				(self presentationScriptingMathods includes: message selector) ==> [
				| argument |
				argument := message arguments first.
				argument isBlock or: [ argument isLiteralNode ] ] ]
			thenCollect: [ :message |
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: anAST
						interval: message	 sourceInterval)
					by: self) ])
				
