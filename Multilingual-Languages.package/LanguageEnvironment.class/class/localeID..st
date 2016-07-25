accessing
localeID: localeID
	self knownEnvironments
		at: localeID ifPresent: [:value | ^ value ];
		at: LocaleID default ifPresent: [ :value | ^ value ].
	
	"Something is wrong"
	self resetKnownEnvironments.
	"Try again with freshly initialized cache"
	^ self localeID: localeID