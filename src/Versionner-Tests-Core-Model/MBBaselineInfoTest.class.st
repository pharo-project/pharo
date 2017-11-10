"
SUnit tests for MBBaselineInfo in Versionner
"
Class {
	#name : #MBBaselineInfoTest,
	#superclass : #MBAbstractVersionInfoTest,
	#instVars : [
		'info',
		'browser'
	],
	#category : #Versionner-Tests-Core-Model
}

{ #category : #running }
MBBaselineInfoTest >> expectedFailures [ 
	true ifTrue: [ ^#() ].
	^ #(#testcalculateInfoList)
]

{ #category : #running }
MBBaselineInfoTest >> setUp [
	| configurationInfo |
	super setUp.
	configurationInfo := MBConfigurationInfo configurationClass: configuration.
	info := configurationInfo baselines detect: [:inf | inf version versionString = '1.0-baseline']

]

{ #category : #tests }
MBBaselineInfoTest >> testCopyBaseline [
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
]

{ #category : #tests }
MBBaselineInfoTest >> testInstantiation [
	|  configurationInfo baseline |

	self should: [ MBBaselineInfo new ] raise: Error.
	self 
		shouldnt: [	configurationInfo := MBConfigurationInfo configurationClass: configuration.
	baseline := configurationInfo baselines detect: [:inf | inf version versionString = '1.0-baseline'].
] 
		raise: Error.
	
	self assert: (baseline versionString = '1.0-baseline').
	self assert: (info configurationInfo configurationClass == (Smalltalk at: #ConfigurationOfDummyConfiguration))
]

{ #category : #tests }
MBBaselineInfoTest >> testcalculateInfoList [

	"Doesn't work!
	 Need to use a real configuration with all information "
	"info := (MBBaselineInfo version: (MBVersionInfo version: (MetacelloMCVersion fromString: '1.0-baseline')))
		configurationInfo: (MBConfigurationInfo configurationClass: configuration).
		
	info calculateInfoList"
]
