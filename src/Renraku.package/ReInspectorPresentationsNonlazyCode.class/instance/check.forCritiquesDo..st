running
check: aMethod forCritiquesDo: aCriticBlock

	(aMethod hasPragmaNamed: #gtInspectorPresentationOrder:) ifFalse: [ ^ self ].
	
	^ (aMethod ast statements
			reject: [ :statement |
				self
					presentation: aMethod ast arguments first
					isTheLeftmostRecepientIn: (statement isReturn
						ifTrue: [ statement value ]
						ifFalse: [ statement ]) ]			
			thenCollect: [ :statement |
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: aMethod
						interval: statement sourceInterval)
					by: self) ])