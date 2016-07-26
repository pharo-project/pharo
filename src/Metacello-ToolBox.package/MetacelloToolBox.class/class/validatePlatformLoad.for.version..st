load validation
validatePlatformLoad: platformAttribute for: configurationBasename version: versionString
	"Load, and run tests for the given version of the configuration. Mark as #stable, if load and tests are clean. Return list of issues."

	<apiDocumentation>
	| issues configurationClass toolbox cleanLoadAndTests stableVersion |
	configurationClass := Smalltalk at: (self configurationNameFrom: configurationBasename) asSymbol.
	cleanLoadAndTests := OrderedCollection new.
	issues := [ MetacelloMCVersionValidator validateConfigurationLoad: configurationClass version: versionString ]
		on: MetacelloCleanNotification
		do: [ :ex | 
			(ex isKindOf: MetacelloCleanLoadAndTestsNotification)
				ifTrue: [ cleanLoadAndTests add: ex version ].
			ex resume ].
	toolbox := self configurationNamed: configurationBasename.
	cleanLoadAndTests isEmpty
		ifTrue: [ ^issues ].
	stableVersion := cleanLoadAndTests last.
	toolbox
		symbolicVersionMethod: #stable;
		addSymbolicSection: platformAttribute version: stableVersion versionString;
		commitMethod.
	stableVersion blessing == #development
		ifTrue: [ 
			issues
				add:
					(MetacelloValidationWarning
						configurationClass: configurationClass
						reasonCode: #stableDevelopmentVersion
						callSite: #validatePlatformLoad:for:version:
						explanation:
							'Development version ' , stableVersion versionString printString , ' is marked as #stableVersion for '
								, configurationClass name asString) ].
	^ issues