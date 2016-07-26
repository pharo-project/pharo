menu
workingCopiesMenu: aMenu


	aMenu target: self;
		add: 'Browse working copy' selector: #browseWorkingCopy.

	1 to: self orderSpecs size do: [ :index | 
		aMenu
			addUpdating: #orderString:
			target: self
			selector: #order:
			argumentList: {index} ].
	^ aMenu