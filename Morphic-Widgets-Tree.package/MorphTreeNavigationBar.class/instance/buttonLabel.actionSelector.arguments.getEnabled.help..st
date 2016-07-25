accessing
buttonLabel: aLabel actionSelector: aSelector arguments: aCollection getEnabled: enableSelector help: helpText

	| b |
	b := (aLabel isString ifTrue: [self basicButton] ifFalse: [self basicIcon])
		actionSelector: aSelector;
		arguments: aCollection;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: helpText;
		yourself.
		aLabel isString 
			ifTrue: [b label: aLabel font: self preferedFont]
			ifFalse: [| f | 
				f := (enableSelector isNil or: [self perform: enableSelector])
					ifTrue: [aLabel]
					ifFalse: [b lock. self asDisabledForm: aLabel].
				b labelGraphic: f].
	^ b
	