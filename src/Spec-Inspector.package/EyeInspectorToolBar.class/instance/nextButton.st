accessing
nextButton
	nextButton
		ifNil: [ 
			nextButton := self instantiate: ButtonPresenter.
			nextButton 
				label: '>';
				action: [ 
					self inspectorWrapper inspectNext.
					self update. ]].
	^ nextButton