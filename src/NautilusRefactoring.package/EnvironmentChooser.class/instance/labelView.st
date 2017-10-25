accessing
labelView
	^ labelView ifNil: [ 
		labelView := self instantiate: LabelPresenter.
		labelView label: 'Environment:' ]