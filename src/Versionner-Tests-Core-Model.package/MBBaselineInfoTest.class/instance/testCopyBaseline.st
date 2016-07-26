tests
testCopyBaseline
	| configurationInfo baselineInfo |
	configurationInfo := MBConfigurationInfo configurationClass: configuration.
	baselineInfo := configurationInfo baselines detect: [:inf | inf version versionString = '1.0-baseline'].
	self assert: (baselineInfo isKindOf: MBBaselineInfo).

	[  (MBCopyBaselineCommand target: baselineInfo for: browser) copyBaselineWithNumber: '1.1'] 
		valueSupplyingAnswer: 'this is my comment'.
 
	self assert: (configuration compiledMethodAt: #baseline11: ifAbsent: []) notNil.
	self assert: (configuration sourceCodeAt: #baseline11:) =   'baseline11: spec
	<version: ''1.1-baseline''>

	spec for: #''common'' do: [
		spec blessing: #''baseline''.
		spec description: ''this is my comment''.
		spec repository: ''http://www.squeaksource.com/Versionner''.
		spec package: ''Collections-Streams''. ].
'