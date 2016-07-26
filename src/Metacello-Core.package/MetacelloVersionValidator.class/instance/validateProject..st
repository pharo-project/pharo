validation
validateProject: project
	| hasNoBaseline hasNoVersion |
	(self visited includes: project configuration class)
		ifTrue: [ ^ self validationReport ].
	self visited add: project configuration class.
	hasNoBaseline := hasNoVersion := true.
	project versions
		do: [ :version | 
			| spec |
			spec := version spec.
			[ 
			spec blessing value == #baseline
				ifTrue: [ 
					hasNoBaseline := false.
					self validateBaselineVersionSpec: spec ]
				ifFalse: [ 
					hasNoVersion := false.
					self validateVersionSpec: spec ] ]
				on: Error
				do: [ :ex | 
					self
						recordValidationError: 'Error composing version ' , version versionString printString , ': ' , ex description
						versionString: version versionString
						callSite: #validateProject:
						reasonCode: #versionCompositionError ] ].
	project symbolicVersionMap keys
		do: [ :symbolicVersion | 
			| version |
			version := [ project version: symbolicVersion ]
				on: Error , MetacelloSymbolicVersionNotDefinedError
				do: [ :ex | 
					"MetacelloSymbolicVersionNotDefinedError are explicitly not defined, so are not validation errors"
					(ex isKindOf: MetacelloSymbolicVersionNotDefinedError)
						ifFalse: [ 
							self
								recordValidationError:
									'symbolic version ' , symbolicVersion printString , ' does not resolve to a literal version.'
								callSite: #validatePragmas
								reasonCode: #cannotResolveVersion ].
					ex return: nil ].
			version ~~ nil
				ifTrue: [ self validateVersionSpecForSymbolicVersion: version spec symbolicVersion: symbolicVersion ] ].
	hasNoVersion
		ifTrue: [ 
			hasNoBaseline
				ifTrue: [ 
					self
						recordValidationError: 'No usable baseline or versions defined.'
						callSite: #validatePragmas
						reasonCode: #noVersionsDefined ]
				ifFalse: [ 
					self
						recordValidationWarning: 'Only baseline defined (no version defined).'
						callSite: #validatePragmas
						reasonCode: #onlyBaselineVersion ] ].
	^ self validationReport