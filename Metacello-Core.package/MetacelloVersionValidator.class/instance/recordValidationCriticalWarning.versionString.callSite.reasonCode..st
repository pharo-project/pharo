accessing
recordValidationCriticalWarning: aString versionString: versionString callSite: callSite reasonCode: aSymbol
	"reasonCodes:
		#packageNameMismatch
		#projectClassNameFileMismatch
		#duplicateVersionDefinitions
		#duplicateAttributeBlocks
	"

	(self criticalWarningReasonCodes includes: aSymbol)
		ifFalse: [ self error: 'Unknown critical warning reason code' ].
	((self exludededValidations at: versionString ifAbsent: [ #() ]) includes: aSymbol)
		ifTrue: [ ^self ].
	(self debug includes: #criticalWarning) ifTrue: [ self halt: 'Debug triggered for critical warning: ', aString ].
	self validationReport
		add:
			(MetacelloValidationCriticalWarning
				configurationClass:
					(self recurse
						ifTrue: [ self configurationClass ]
						ifFalse: [ nil ])
				reasonCode: aSymbol
				callSite: callSite
				explanation: aString)