running
check: aMethod forCritiquesDo: aCriticBlock

	(aMethod hasPragmaNamed: #gtInspectorPresentationOrder:) ifFalse: [ ^ self ].
	
	^ (aMethod ast sendNodes
			reject: [ :message |
				(self presentationScriptingMathods includes: message selector) ==> [
				| argument |
				argument := message arguments first.
				argument isBlock or: [ argument isLiteralNode ] ] ]
			thenCollect: [ :message |
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: aMethod
						interval: message	 sourceInterval)
					by: self) ])
				
