accessing
previousButton
	previousButton
		ifNil: [ 
			previousButton := self instantiate: ButtonModel.
			previousButton label: '<'.
			previousButton
				action: [ 
					self inspectorWrapper inspectPrevious.
					self update. ] ].
	^ previousButton