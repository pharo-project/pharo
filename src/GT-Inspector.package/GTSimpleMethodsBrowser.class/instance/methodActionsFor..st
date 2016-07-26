building
methodActionsFor: aMethod
	^ aMethod selector isUnary
		ifTrue: [ Array with: self evaluationAction ]
		ifFalse: [ #() ]