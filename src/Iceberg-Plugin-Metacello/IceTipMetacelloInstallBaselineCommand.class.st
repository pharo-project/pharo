"
I'm a general command to install metacello baselines.
"
Class {
	#name : #IceTipMetacelloInstallBaselineCommand,
	#superclass : #IceTipPackageCommand,
	#instVars : [
		'package'
	],
	#category : #'Iceberg-Plugin-Metacello'
}

{ #category : #accessing }
IceTipMetacelloInstallBaselineCommand class >> baselinePrefix [
	^ 'BaselineOf'
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineCommand class >> isAbstract [

	^ self == IceTipMetacelloInstallBaselineCommand
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> allBaselines [
	^ ((self workingCopy packages
		select: [ :each | each name beginsWith: self class baselinePrefix ])
		sorted: [ :a :b | a name < b name ])
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> basicInstallBaseline: icePackage groups: anArray [
	| packageDir baselineName sourceDirectory|

	sourceDirectory := icePackage repository project sourceDirectory.
	sourceDirectory ifEmpty: [ sourceDirectory := '.'].

	packageDir := (icePackage repository location / sourceDirectory) fullName.
	baselineName := icePackage name allButFirst: self class baselinePrefix size.
	Metacello new
		repository: 'gitlocal://', packageDir;
		baseline: baselineName;
		onUpgrade: [ :e | | policy |
			policy := self chooseUpgradePolicyFor: e.
			policy ifNotNil: [ e perform: policy ] ];
		onConflict: [ :e | | policy |
			policy := self chooseConflictPolicyFor: e.
			policy ifNotNil: [ e perform: policy ] ];
		load: anArray
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> chooseConflictPolicyFor: aMetacelloUpgradeRequest [
	| projectName optionA optionB context |

	context := IceMetacelloLoadSessionContext value.
	projectName := aMetacelloUpgradeRequest projectName.
	optionA := aMetacelloUpgradeRequest newProjectRegistration printString.
	optionB := aMetacelloUpgradeRequest existingProjectRegistration printString.
	
	^ context 
		recordChoiceProject: projectName 
		optionA: optionA 
		optionB: optionB
		ifAbsentAnswer: [ 	
			self 
				chooseConflictPolicyProject: projectName 
				optionA: optionA 
				optionB: optionB ]
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> chooseConflictPolicyProject: projectName optionA: optionA optionB: optionB [

	^ UIManager default 
		chooseFrom: {
			'Use INCOMING version {1}' format: { optionA }. 
			'Use LOADED version {1}' format: { optionB } }
		values: { 
			#useIncoming. 
			#useLoaded }
		message: ('There are conflicting versions of project "{1}".' format: { projectName })
		title: 'Conflicting project versions'
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> chooseUpgradePolicyFor: aMetacelloUpgradeRequest [
	| projectName optionA optionB context |

	context := IceMetacelloLoadSessionContext value.
	projectName := aMetacelloUpgradeRequest projectName.
	optionA := aMetacelloUpgradeRequest newProjectRegistration printString.
	optionB := aMetacelloUpgradeRequest existingProjectRegistration printString.
	
	^ context 
		recordChoiceProject: projectName 
		optionA: optionA 
		optionB: optionB
		ifAbsentAnswer: [ 				
			self 
				chooseUpgradePolicyProject: projectName 
				optionA: optionA 
				optionB: optionB ]
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> chooseUpgradePolicyProject: projectName optionA: optionA optionB: optionB [

	^ UIManager default 
		chooseFrom: {
			'Use INCOMING version {1}' format: { optionA }. 
			'Use LOADED version {1}' format: { optionB } }
		values: { 
			#useIncoming. 
			#useLoaded }
		message: ('There is already a project "{1}" in this installation.' format: { projectName })
		title: 'Duplicated project!'
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> installBaseline: icePackage [ 
	^ self installBaseline: icePackage groups: #()
]

{ #category : #private }
IceTipMetacelloInstallBaselineCommand >> installBaseline: icePackage groups: anArray [
	IceTipStandardAction new
		repository: self repositoryModel entity;
		message: ('Installing baseline {1} of {2}' format: { 
			icePackage name allButFirst: self class baselinePrefix size. 
			self repositoryModel name });
		onSuccessRepositoryModified;
		execute: [ 
			Iceberg announcer 
				suspendAllForRepository: self repositoryModel entity
				while: [ self basicInstallBaseline: icePackage groups: anArray ] ]
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineCommand >> package [
	^ package
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineCommand >> package: anIcePackage [
	package := anIcePackage
]

{ #category : #execution }
IceTipMetacelloInstallBaselineCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.	
	package := self packageModel entity
]

{ #category : #accessing }
IceTipMetacelloInstallBaselineCommand >> repositoryModel: aRepositoryModel [
	repositoryModel := aRepositoryModel
]
