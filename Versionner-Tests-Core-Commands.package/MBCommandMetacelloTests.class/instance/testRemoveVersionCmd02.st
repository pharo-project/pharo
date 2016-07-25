tests
testRemoveVersionCmd02
	"Remove a bleedingEdge version that is imported ... should abort the operation"

	| configName class configurationRoot configInfo versionInfo cmd versionString |
	configName := 'ConfigurationOfMBFooIssue117'.
	self tearDownPackages addAll: {configName}.
	gofer version: 'ConfigurationOfMBFooIssue117-dkh.1'.
	gofer load.
	class := Smalltalk at: configName asSymbol.
	versionString := (class project version: #bleedingEdge) versionString.
	configurationRoot := MBConfigurationRoot new.
	configurationRoot register.
	configInfo := configurationRoot configurationInfoFor: class.
	versionInfo := configInfo baselines detect: [ :each | each versionString = versionString ].
	cmd := VSRemoveCommand target: versionInfo for: self.
	[ cmd execute ]
		valueSupplyingAnswers:
			{{'*is imported by another version*'.
			true}}.
	self assert: versionString = (class project version: #bleedingEdge) versionString