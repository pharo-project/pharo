running
check: anAST forCritiquesDo: aCriticBlock


	anAST pragmaNamed: #gtInspectorPresentationOrder: ifAbsent: [ ^ self ].
	
	^ (anAST statements
			reject: [ :statement |
				self
					presentation: anAST arguments first
					isTheLeftmostRecepientIn: (statement isReturn
						ifTrue: [ statement value ]
						ifFalse: [ statement ]) ]			
			thenCollect: [ :statement |
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: anAST
						interval: statement sourceInterval)
					by: self) ])