accessing
settingsButtonLabel
	
	^ self sendUsageData 
		ifTrue: [ 'Change preferences' ]
		ifFalse: [  'Go to settings' ]