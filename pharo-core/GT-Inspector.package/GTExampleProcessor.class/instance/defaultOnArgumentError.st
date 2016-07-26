accessing-defaults
defaultOnArgumentError
	^ [ :aProcessor :anExample :someArguments | 
		aProcessor handleArgumentErrorFor: anExample withArguments: someArguments.
		nil ]