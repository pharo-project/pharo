accessing
nextButton
	nextButton
		ifNil: [ 
			nextButton := self instantiate: ButtonModel.
			nextButton 
				label: '>';
				action: [ 
					self inspectorWrapper inspectNext.
					self update. ]].
	^ nextButton