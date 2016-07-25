load validation
validatePlatformLoad: platformAttribute for: configurationBasename
	"Validate, Load, and run tests for all versions of the given configuration. Return list of issues."

	<apiDocumentation>
	| issues configurationClass toolbox cleanDevelopmentLoads cleanLoadAndTests stableVersion |
	configurationClass := Smalltalk at: (self configurationNameFrom: configurationBasename) asSymbol.
	cleanDevelopmentLoads := OrderedCollection new.
	cleanLoadAndTests := OrderedCollection new.
	issues := [ MetacelloMCVersionValidator validateConfigurationLoad: configurationClass ]
		on: MetacelloCleanNotification
		do: [ :ex | 
			(ex isKindOf: MetacelloCleanLoadAndTestsNotification)
				ifTrue: [ cleanLoadAndTests add: ex version ].
			((ex isKindOf: MetacelloCleanLoadNotification) and: [ ex version blessing == #development ])
				ifTrue: [ cleanDevelopmentLoads add: ex version ].
			ex resume ].
	(cleanDevelopmentLoads isEmpty and: [ cleanLoadAndTests isEmpty ])
		ifTrue: [ 
			self inform: 'Failed validation with no versions load clean'.
			^ issues ].
	toolbox := self configurationNamed: configurationBasename.
	cleanLoadAndTests notEmpty
		ifTrue: [ 
			toolbox
				symbolicVersionMethod: #stable;
				addSymbolicSection: platformAttribute version: (stableVersion := cleanLoadAndTests last) versionString;
				commitMethod ].
	stableVersion notNil
		ifTrue: [ 
			stableVersion blessing == #development
				ifTrue: [ 
					issues
						add:
							(MetacelloValidationWarning
								configurationClass: configurationClass
								reasonCode: #stableDevelopmentVersion
								callSite: #validatePlatformLoad:for:
								explanation:
									'Development version ' , stableVersion versionString printString , ' is marked as #stableVersion for '
										, configurationClass name asString) ].
			cleanDevelopmentLoads := cleanDevelopmentLoads select: [ :version | version > stableVersion ] ].
	cleanDevelopmentLoads notEmpty
		ifTrue: [ 
			toolbox
				symbolicVersionMethod: #development;
				addSymbolicSection: platformAttribute version: cleanDevelopmentLoads last versionString;
				commitMethod ]
		ifFalse: [ 
			toolbox
				symbolicVersionMethod: #development;
				addSymbolicSection: platformAttribute version: #notDefined;
				commitMethod ].
	^ issues