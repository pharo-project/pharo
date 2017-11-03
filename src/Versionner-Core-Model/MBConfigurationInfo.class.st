"
A MBConfigurationInfo holds information on a specific configuration, i.e. on the ConfigurationOf class of a project.

Instance Variables
	baselines:		list of baselines (MBBaseLineInfo instances) of this configuration.
	branch:			??
	configurationClass:	the configuration class (ConfigurationOf instance).
	configurationRoot:		a link to the root object with all configurations.
	currentVersion:		??
	trimVersionLists:		true if ??
	versions:			list of versions (MBVersionInfo instances) of this configuration.
	workingCopy:	the Metacello working copy

"
Class {
	#name : #MBConfigurationInfo,
	#superclass : #MBAbstractInfoList,
	#instVars : [
		'configurationRoot',
		'branch',
		'configurationClass',
		'currentVersion',
		'baselines',
		'versions',
		'workingCopy',
		'trimVersionLists'
	],
	#category : #Versionner-Core-Model
}

{ #category : #'instance creation' }
MBConfigurationInfo class >> configurationClass: aConfiguration [
	^ super new 
		configurationClass: aConfiguration;
		yourself
]

{ #category : #accessing }
MBConfigurationInfo class >> helpLabel [

	^'Configuration'
]

{ #category : #'instance creation' }
MBConfigurationInfo class >> new [
	self error: 'Please, use #configuration: to instantiate ', self name printString
]

{ #category : #accessing-computed }
MBConfigurationInfo >> baselines [ 
	baselines ifNotNil: [ ^baselines ].
	baselines := self baselinesFor: self project.
	^baselines 
]

{ #category : #accessing }
MBConfigurationInfo >> baselinesFor: aMetacelloProject [
	"Return the list of  BaselineInfo for the configuration class passed as parameter"

	"The returned collection is ordered. Labeled baselines are first, numbered baselines are last"

	| existingBaselines numberedBaselines labeledBaselines |
	existingBaselines := aMetacelloProject versions select: [ :vrsn | vrsn blessing == #baseline ].
	existingBaselines := existingBaselines
		collect: [ :vrsn | 
			(MBBaselineInfo version: vrsn)
				configurationInfo: self;
				yourself ].
	numberedBaselines := existingBaselines select: [ :bInfo | ($0 to: $9) includes: bInfo name first ].
	labeledBaselines := existingBaselines copyWithoutAll: numberedBaselines.
	^ labeledBaselines asArray , numberedBaselines asArray reversed
]

{ #category : #accessing }
MBConfigurationInfo >> branch [

	branch ifNil: [ branch := self extractBranchFrom: self workingCopy ].
	^branch
]

{ #category : #converting }
MBConfigurationInfo >> buildStringOrText [
	| string cv specVersion attributes wc |
	attributes := OrderedCollection new.
	[ string := super buildStringOrText ]
		on: Error
		do: [ :ex | 
			attributes add: TextEmphasis struckOut.
			^ Text string: self name asString attributes: attributes ].
	((wc := self workingCopy) notNil and: [ wc needsSaving ])
		ifTrue: [ attributes add: TextEmphasis underlined ].
	self currentVersion. "prime the pump"
	currentVersion == #notLoaded
		ifTrue: [ 
			attributes notEmpty
				ifTrue: [ ^ Text string: string attributes: attributes ].
			^ string ].
	super isDirty | self currentVersionMismatch
		ifTrue: [ attributes add: TextEmphasis bold ].
	self currentVersion versionStatus == #somethingLoaded
		ifTrue: [ attributes add: TextColor red ].
	attributes notEmpty
		ifTrue: [ ^ Text string: string attributes: attributes ].
	^ string
]

{ #category : #private }
MBConfigurationInfo >> calculateInfoList [
	| answer list cvs cv |
	answer := OrderedCollection new.
	[self project ifNil: [ ^answer ]] on: Error do: [:ex | ^answer ].
	answer add: (MBLabelInfo new name: ' -- baselines --').
	answer addAll: (self trimVersionInfos: self baselines).
	answer add: (MBLabelInfo new name: '').
	answer add: (MBLabelInfo new name: ' -- versions --').
	answer addAll: (self trimVersionInfos: self versions).
	answer add: (MBLabelInfo new name: '').
	cvs := ''.
	(cv := self currentVersion) ~~ nil
		ifTrue:  [cvs := cv versionString, ' '].
	(list := self projects) notEmpty
		ifTrue: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'projects --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self packages) notEmpty
		ifTrue: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'packages --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self groups) notEmpty
		ifTrue: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'groups --').
			answer addAll: list].
	^ answer
]

{ #category : #commands }
MBConfigurationInfo >> cmdBrowseConfiguration [
	<command>
	^ VSBrowseConfigurationCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdCatalogAdder [
	<command>
	^ VSCatalogAdderCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdCommitMajorVersion [
	<command>
	^ VSCommitMajorCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdCommitMinorVersion [
	<command>
	^ VSCommitMinorCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdCommitPatchVersion [
	<command>
	^ VSCommitPatchCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdOpenRepository [
	<command>
	^ VSOpenRepositoryCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdSaveConfiguration [
	<command>
	^ MBSaveConfigurationCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdUnloadConfiguration [
	<command>
	^ VSUnloadConfigurationCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdUpdateFromItsRepository [
	<command>
	^ VSUpdateFromItsRepositoryCommand
]

{ #category : #commands }
MBConfigurationInfo >> cmdValidateConfiguration [
	<command>
	^ VSValidateConfigurationCommand
]

{ #category : #accessing }
MBConfigurationInfo >> configurationClass [
	"Returns the class configurationOfXXX associted to the configuration info (wrapper for the UI)"

	^ configurationClass
]

{ #category : #accessing }
MBConfigurationInfo >> configurationClass: aConfigurationClass [
	configurationClass := aConfigurationClass.
	self name: configurationClass name asString
]

{ #category : #updating }
MBConfigurationInfo >> configurationClassModified [

	self fullRecalculate 
]

{ #category : #accessing }
MBConfigurationInfo >> configurationInfo [

	^self 
]

{ #category : #accessing }
MBConfigurationInfo >> configurationRoot [
	^ configurationRoot
]

{ #category : #accessing }
MBConfigurationInfo >> configurationRoot: anObject [
	configurationRoot := anObject
]

{ #category : #accessing-computed }
MBConfigurationInfo >> currentVersion [
	"Return the currentVersion of the configuration ... nil if not loaded."

	currentVersion == #notLoaded
		ifTrue: [ ^ nil ].
	(currentVersion notNil and: [ currentVersion ~~ #recalculate])
		ifTrue: [ ^ currentVersion ].
	packages := projects := groups := text := nil.
	self configurationRoot ifNil: [  ^ currentVersion ].
	(currentVersion := self project currentVersion)
		ifNil: [ 
			currentVersion := #notLoaded.
			self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).
			^ nil ].
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).
	^ currentVersion
]

{ #category : #accessing-computed }
MBConfigurationInfo >> currentVersion: aVersion [

	currentVersion  := aVersion.
	branch := baselines := versions := infoList := packages := projects := groups := text := nil.
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).
	self announcer announce: (MBInfoListChanged changed: self).

]

{ #category : #testing }
MBConfigurationInfo >> currentVersionMismatch [
	^ (self projects anySatisfy: [ :each | each currentVersionMismatch ])
		or: [ self packages anySatisfy: [ :each | each currentVersionMismatch ] ]
]

{ #category : #utils }
MBConfigurationInfo >> extractBranchFrom: aWorkingCopy [
	| ref |
	(aWorkingCopy isNil or: [ aWorkingCopy ancestors isEmpty ])
		ifTrue: [ ^ '' ].
	ref := GoferVersionReference name: aWorkingCopy ancestors first name.
	^ ref branch
]

{ #category : #updating }
MBConfigurationInfo >> fullRecalculate [

	super fullRecalculate.
	(currentVersion == #recalculate or: [ currentVersion isNil ]) ifTrue: [^self ].
	currentVersion := #recalculate.

]

{ #category : #testing }
MBConfigurationInfo >> hasVersion: aVersionString [
 	[ self project version: aVersionString ]
		on: MetacelloSymbolicVersionDoesNotExistError
		do:  [ ^ false ].
	^ true
]

{ #category : #testing }
MBConfigurationInfo >> isDirty [
	| wc |
	^ [super isDirty or: [ (wc := self workingCopy) notNil and: [ wc needsSaving ]]] on:Error do:[false].
]

{ #category : #printing }
MBConfigurationInfo >> printOn: aStream [
	| cv nameString wc refString branchString |
	[self name
		ifNil: [ 
			aStream nextPutAll: ''.
			^ self ].
	nameString := self name configurationBaseName.
	(refString := self extractPackageSignatureFrom: (wc := self workingCopy)) notEmpty
		ifTrue: [ refString := '(' , refString , ')' ].
	(branchString := self branch) notEmpty
		ifTrue: [ nameString := nameString, '.', branchString ].
	self isDirty
		ifTrue: [ aStream nextPutAll: '* ' ].
	aStream nextPutAll: nameString.
	(cv := self currentVersion) notNil
		ifTrue: [ aStream nextPutAll: ' ' , cv versionString ].
	aStream nextPutAll: refString.] on:Error do:[].
]

{ #category : #accessing }
MBConfigurationInfo >> project [
	"Answer the MetacelloProject associated with the receiver."

	^ self configurationClass project
]

{ #category : #updating }
MBConfigurationInfo >> recalculate [
	branch := workingCopy := baselines := versions := nil.
	super recalculate.
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).

]

{ #category : #updating }
MBConfigurationInfo >> recalculateIfInterestedInConfigurationInfo: configInfo for: operation [

	projects ifNil: [ ^self ].
	self projects detect: [ :info | info interestedInConfigurationInfo: configInfo ] ifNone: [ ^self ].
	infoList := text := projects := nil.
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self)
]

{ #category : #updating }
MBConfigurationInfo >> recalculateIfInterestedInPackageNamed: packageName for: operation [
	| recalculate |
	recalculate := super recalculateIfInterestedInPackageNamed: packageName for: operation.
	(recalculate not and: [ baselines notNil ])
		ifTrue: [ 
			(self baselines detect: [ :info | info recalculateIfInterestedInPackageNamed: packageName for: operation ] ifNone: [  ])
				ifNotNil: [ recalculate := true ] ].
	(recalculate not and: [ versions notNil ])
		ifTrue: [ 
			(self versions detect: [ :info | info recalculateIfInterestedInPackageNamed: packageName for: operation ] ifNone: [  ])
				ifNotNil: [ recalculate := true ] ].
	(recalculate and: [ operation ~~ #modified ])
		ifTrue: [ 
			currentVersion == #notLoaded
				ifTrue: [ currentVersion := nil ].
			self fullRecalculate ]
]

{ #category : #updating }
MBConfigurationInfo >> recalculateSilently [
	branch := workingCopy := baselines := versions := nil.
	super recalculate.

]

{ #category : #initialize-release }
MBConfigurationInfo >> release [

	baselines := versions := nil.
	super release.	
]

{ #category : #accessing }
MBConfigurationInfo >> repositories [
	"Return the repositories associated with the configuration."
	| mcPackage |
	
	(mcPackage := self project projectPackage)
		ifNil: [ ^nil ].
	^ mcPackage workingCopy
		ifNil: [ nil ]
		ifNotNil: [ :wc | wc repositoryGroup repositories ]
]

{ #category : #private }
MBConfigurationInfo >> trimVersionInfos: versionInfos [

	| trimmed count marker cv |
	self trimVersionLists ifFalse: [ ^versionInfos ].
	versionInfos size <= 10 ifTrue: [ ^versionInfos ].
	trimmed := (versionInfos copyFrom: 1 to: 5) asOrderedCollection.
	marker := count := 0.
	cv := self currentVersion.
	 6 to: versionInfos size do: [:index | | info |
		info := versionInfos at: index.
		info isStable | info isBleedingEdge | info isDevelopment | info hasValidationIssues | (cv = info version)
			ifTrue: [ 
				count - 1 >= marker ifTrue: [ trimmed add:( MBLabelInfo new name: '..') ].
				trimmed add: info.
				marker := count. ].
		count := count + 1 ].
	count - 1 >= marker ifTrue: [ trimmed add:( MBLabelInfo new name: '..') ].
	^trimmed
]

{ #category : #accessing }
MBConfigurationInfo >> trimVersionLists [

	trimVersionLists == nil ifTrue: [ trimVersionLists := true ].
	^trimVersionLists
]

{ #category : #accessing }
MBConfigurationInfo >> trimVersionLists: aBool [

	trimVersionLists := aBool
]

{ #category : #actions }
MBConfigurationInfo >> validate [
	validationResult == nil
		ifTrue: [ 
			validationIcon := nil.
			validationResult := MetacelloToolBox validateConfiguration: self configurationClass ].
	^ validationResult
]

{ #category : #actions }
MBConfigurationInfo >> validateFull [

	infoList := baselines := versions := nil.
	(self baselines, self versions) do: [:version | version validateFull ].
	^super validateFull.

]

{ #category : #accessing }
MBConfigurationInfo >> version [

	^self currentVersion
]

{ #category : #accessing-computed }
MBConfigurationInfo >> versionForInfoList [

	^self currentVersion 
]

{ #category : #accessing-computed }
MBConfigurationInfo >> versions [
	"Return the versions of the configuration"
	| versionList devVersionString |
	
	versions ifNotNil: [ versions ifNotEmpty: [ ^ versions ] ].
	versionList := self project map values.
	devVersionString := self project symbolicVersionMap at: #development ifAbsent: [ '' ].
	versionList reject: [ :vrsn | (vrsn blessing == #baseline and: [(vrsn name == devVersionString) not]) or: [ vrsn blessing == #broken ] ].
	
	versions := (versionList asArray sort: [:a :b | a > b ])
		collect: [ :vrsn | 
			(MBVersionInfo version: vrsn)
				configurationInfo: self;
				yourself ].
	^ versions
]

{ #category : #accessing }
MBConfigurationInfo >> workingCopy [
	| pkg |
	workingCopy ~~ nil ifTrue: [ ^workingCopy ].
	pkg := MCPackage named: self configurationClass category asString.
	pkg  hasWorkingCopy ifFalse: [ ^nil ].
	^workingCopy := pkg workingCopy
]
