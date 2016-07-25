loading
validateCleanLoadAndTestsForVersion: version loads: loadList
	| cleanLoad cleanTests |
	cleanTests := cleanLoad := false.
	[ 
	self validateVersionLoad: version loads: loadList.
	cleanLoad := true ]
		on: Error , Warning
		do: [ :ex | 
			(ex isKindOf: Error)
				ifTrue: [ 
					self
						recordValidationError:
							'Error while loading version ' , version versionString printString , ' in ' , self configurationClass name asString
								, ' ' , ex description
						versionString: version versionString
						callSite: #validateCleanLoadAndTestsForVersion:loads:
						reasonCode: #loadError ].
			(ex isKindOf: Warning)
				ifTrue: [ 
					(ex isKindOf: Deprecation)
						ifTrue: [ 
							self
								recordValidationCriticalWarning:
									'Deprecation while loading version ' , version versionString printString , ' in '
										, self configurationClass name asString , ' ' , ex description
								versionString: version versionString
								callSite: #validateCleanLoadAndTestsForVersion:loads:
								reasonCode: #loadDeprecation ]
						ifFalse: [ 
							self
								recordValidationWarning:
									'Warning while loading version ' , version versionString printString , ' in ' , self configurationClass name asString
										, ' ' , ex description
								versionString: version versionString
								callSite: #validateCleanLoadAndTestsForVersion:loads:
								reasonCode: #loadWarning.
							Smalltalk
								at: #UndeclaredVariableWarning
								ifPresent: [ :undeclaredWrning | 
									(ex isKindOf: undeclaredWrning)
										ifTrue: [ ex resume: true ] ].
							ex resume ] ] ].
	cleanLoad
		ifTrue: [ 
			cleanTests := [ self validateVersionTests: version ]
				on: Deprecation
				do: [ :ex | 
					| message |
					message := 'Deprecation warning while running tests for version ' , version versionString printString , ' in '
						, self configurationClass name asString , ' ' , ex description.	"Deprecation warning for release tests is the same as a test failure"
					self
						recordValidationCriticalWarning: message
						versionString: version versionString
						callSite: #validateCleanLoadAndTestsForVersion:loads:
						reasonCode: #testDeprecation.
					ex return: false ].
			cleanTests
				ifTrue: [ MetacelloCleanLoadAndTestsNotification signal: version ]
				ifFalse: [ MetacelloCleanLoadNotification signal: version ] ]