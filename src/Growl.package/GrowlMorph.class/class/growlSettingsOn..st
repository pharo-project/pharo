settings
growlSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #growl)
		label: 'Popup notifiaction';  
		parent: #appearance;
		description: 'All settings concerned with the notifications popup look''n feel';
		with: [
			(aBuilder pickOne: #position)
				label: 'Popup position';
				target: self;
				default: #topRight;
				order: 1;
				domainValues: self growlPositionChoices.]