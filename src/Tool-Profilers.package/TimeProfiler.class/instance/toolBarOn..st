UI specific
toolBarOn: aWindow
	| uiTheme toolBar |
	uiTheme := Smalltalk ui theme.
	toolBar := toolBar := aWindow
		newToolbar:
			{((uiTheme
				newButtonIn: World
				for: self
				getState: nil
				action: #profileIt
				arguments: {}
				getEnabled: nil
				getLabel: nil
				help: nil)
				label:
					(uiTheme
						newRowIn: World
						for:
							{(AlphaImageMorph new
								image: (Smalltalk ui icons iconNamed: #smallDoItIcon)).
							(uiTheme buttonLabelForText: 'Profile it' translated)});
				yourself).
			aWindow newToolSpacer.
			aWindow newToolSpacer.
			(aWindow newLabel: 'Min time % ' translated).
			(uiTheme
				newTextEntryIn: aWindow
				for: self
				get: #minPercentageToExpand
				set: #minPercentageToExpand:
				class: Float
				getEnabled: nil
				help: nil).
			aWindow newToolSpacer.
			aWindow newToolSpacer.
			(aWindow newLabel: 'Threshold: ' translated).
			(uiTheme
				newTextEntryIn: aWindow
				for: self
				get: #threshold
				set: #threshold:
				class: Integer
				getEnabled: nil
				help: nil).
			aWindow newToolSpacer.
			aWindow newToolSpacer.
			(aWindow
				newDropListFor: self
				list: #whatToShowList
				getSelected: #whatToShow
				setSelected: #whatToShow:
				getEnabled: nil
				useIndex: false
				help: 'Which kind of result').
			aWindow newToolSpacer.
			aWindow newToolSpacer.
			(uiTheme
				newCheckboxIn: World
				for: self
				getSelected: #reportOtherProcesses
				setSelected: #reportOtherProcesses:
				getEnabled: nil
				label: 'All processes'
				help: nil).
			aWindow newToolSpacer}.
	^ toolBar