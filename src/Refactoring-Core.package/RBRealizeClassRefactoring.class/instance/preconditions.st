preconditions
preconditions
	^ RBCondition
		withBlock: [
			(self theClass withAllSubclasses
				detect: [ :each | 
					(each whichSelectorsReferToSymbol: #subclassResponsibility) notEmpty 
						or: [ (each theMetaClass whichSelectorsReferToSymbol: #subclassResponsibility) notEmpty ] ]
				ifNone: [ nil ]) isNil ]
		errorString: self theClass printString , ' is abstract or has abstract subclasses.'