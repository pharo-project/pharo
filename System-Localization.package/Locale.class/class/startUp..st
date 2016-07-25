system startup
startUp: resuming
	| newID |
	resuming ifFalse: [^self].
	DateAndTime localTimeZone: self localTimeZone.
	(self activated)
		ifTrue: [
			newID := self current determineLocaleID.
			newID ~= LocaleID current
				ifTrue: [self switchToID: newID]]