running
check: aMethod forCritiquesDo: aCriticBlock ifNone: alternativeBlock

	| encounteredCritic |
	encounteredCritic := false.

	aMethod ast nodesDo: [ :node |
		(node isVariable and: [
			node binding ifNil: [ node doSemanticAnalysis ].
			node isGlobal ]) ifTrue: [ 
			| message |
			message := node parent.
			(message isMessage and: [
			 self check: message selector forDeprecationIn: node name ]) ifTrue: [ 
				encounteredCritic := true.
				aCriticBlock cull: (ReTrivialCritique
					withAnchor: (ReIntervalSourceAnchor
						entity: aMethod
						interval: message sourceInterval)
					by: self)
				
				 ] ] ].
	
	encounteredCritic ifFalse: alternativeBlock 