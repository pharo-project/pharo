accessing-defaults
defaultOnDependencyError
	^ [ :aProcessor :anExample :anException | 
		aProcessor handleDependencyErrorFor: anExample withException: anException.
		nil ]