tests
testRemoveVersionCmd01
	"Issue 117: Removing a version does not update the #development/#stable symbolic versions."

	| configName class configurationRoot configInfo versionInfo cmd |
	configName := 'ConfigurationOfMBFooIssue117'.
	self tearDownPackages addAll: {configName}.
	gofer version: 'ConfigurationOfMBFooIssue117-dkh.1'.
	gofer load.
	class := Smalltalk at: configName asSymbol.
	self assert: (class project version: #development) versionString = '1.0'.
	self assert: (class project version: #stable) versionString = '1.0'.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	configInfo := configurationRoot configurationInfoFor: class.
	versionInfo := configInfo versions detect: [ :each | each versionString = '1.0' ].
	cmd := VSRemoveCommand target: versionInfo for: self.
	[ cmd execute ]
		valueSupplyingAnswers:
			{{'Are you sure you want to remove*'.
			true}}.
	self should: [ class project version: #development ] raise: MetacelloSymbolicVersionNotDefinedError.
	self should: [ class project version: #stable ] raise: MetacelloSymbolicVersionNotDefinedError.
	self should: [ class project version: '1.0' ] raise: MetacelloVersionDoesNotExistError