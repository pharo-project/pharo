accessing
labelView
	^ labelView ifNil: [ 
		labelView := self instantiate: LabelModel.
		labelView label: 'Environment:' ]