accessing
recordValidationError: aString versionString: versionString callSite: callSite reasonCode: aSymbol
	"reasonCodes:
		#noVersionForSymbolicVersion
		#duplicateNames
		#shadowedNames
		#invalidDoItSelector
		#invalidVersionString
		#missingVersionImport
		#projectCreationError
		#noVersionsDefined
		#cannotResolveVersion
		#incompleteProjectSpec
		#incorrectVersionString
	"

	(self errorReasonCodes includes: aSymbol)
		ifFalse: [ self error: 'Unknown error reason code' ].
	((self exludededValidations at: versionString ifAbsent: [ #() ]) includes: aSymbol)
		ifTrue: [ ^self ].
	(self debug includes: #error) ifTrue: [ self halt: 'Debug triggered for error: ', aString ].
	self validationReport
		add:
			(MetacelloValidationError
				configurationClass:
					(self recurse
						ifTrue: [ self configurationClass ]
						ifFalse: [ nil ])
				reasonCode: aSymbol
				callSite: callSite
				explanation: aString)