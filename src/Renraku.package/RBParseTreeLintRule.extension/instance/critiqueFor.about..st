*renraku
critiqueFor: aNode about: aMethod
	| anchor | 

	anchor := (aNode isKindOf: RBProgramNode)
		ifFalse: [ ReSourceAnchor entity: aMethod ]
		ifTrue: [
			ReIntervalSourceAnchor
				entity: aMethod
				interval: aNode sourceInterval ].
			
	^ ReTrivialCritique
		withAnchor: anchor 
		by: self