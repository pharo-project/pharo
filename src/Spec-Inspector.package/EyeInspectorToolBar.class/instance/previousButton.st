accessing
previousButton
	previousButton
		ifNil: [ 
			previousButton := self instantiate: ButtonPresenter.
			previousButton label: '<'.
			previousButton
				action: [ 
					self inspectorWrapper inspectPrevious.
					self update. ] ].
	^ previousButton