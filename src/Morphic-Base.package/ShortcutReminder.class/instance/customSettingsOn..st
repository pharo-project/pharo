settings
customSettingsOn: aBuilder

	(aBuilder pickOne: #position)
		order: 1;
		target: self;
		label: 'Position in World';
		description: 'Select one of the positons available to specify where the shortcut notification morph will be shown';
		domainValues: self positions.
		
	(aBuilder pickOne: #textSize) 
		order: 2;
		target: self;
		label: 'Size of the reminder';
		description: 'Set the size (small medium or large) of the reminder morph shown';
		domainValues: self sizes.
		
	(aBuilder setting: #textColor)
		order: 3;
		target: self;
		label: 'Text color';
		default: self defaultTextColor.
		
	(aBuilder setting: #backgroundColor)
		target: self;
		label: 'Background color';
		default: self defaultBackgroundColor.
		
	(aBuilder range: #limit) 
		order: 6;
		target: self; 
      label: 'Notification limit' ;
		description: 'Set the limit of how many notifications per menu entry one should be reminded about. Setting the value to -1 means that it will allways remind you' ;
		default: self defaultLimit;
		range: (-1 to: 100).
		
	(aBuilder group: #'Reset count')
		order: 7;
		target: self;
		label: 'Reset the reminder count';
		dialog: [ self createResetCountButtonMorph ].