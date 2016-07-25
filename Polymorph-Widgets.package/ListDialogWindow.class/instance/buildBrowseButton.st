item creation
buildBrowseButton

	^ (PluggableButtonMorph
		on: self
		getState: #state
		action: #browseAction)
		label: 'Browse';
		yourself