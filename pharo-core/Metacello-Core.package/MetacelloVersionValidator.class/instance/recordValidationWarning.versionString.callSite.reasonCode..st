accessing
recordValidationWarning: aString versionString: versionString callSite: callSite reasonCode: aSymbol
	"reasonCodes:
		#onlyBaselineVersion
		#noVersionSpecified
	"

	(self warningReasonCodes includes: aSymbol)
		ifFalse: [ self error: 'Unknown warning reason code' ].
	((self exludededValidations at: versionString ifAbsent: [ #() ]) includes: aSymbol)
		ifTrue: [ ^self ].
	(self debug includes: #warning) ifTrue: [ self halt: 'Debug triggered for critical warning: ', aString ].
	self validationReport
		add:
			(MetacelloValidationWarning
				configurationClass:
					(self recurse
						ifTrue: [ self configurationClass ]
						ifFalse: [ nil ])
				reasonCode: aSymbol
				callSite: callSite
				explanation: aString)