settings
growlSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #growl)
		label: 'Popup notifiaction' translated;  
		parent: #appearance;
		description: 'All settings concerned with the notifications popup look''n feel' translated;
		with: [
			(aBuilder pickOne: #position)
				label: 'Popup position' translated;
				target: self;
				default: #topRight;
				order: 1;
				domainValues: self growlPositionChoices.]