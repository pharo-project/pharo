accessing
titleAction

	^ self titleActionBlock 
		ifNil: [ nil ]
		ifNotNil: [ :aBlock |
			aBlock glamourValue: self ]