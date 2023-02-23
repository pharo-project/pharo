"
A SystemSettingsTest is a test class for testing the behavior of SystemSettings
"
Class {
	#name : #SystemSettingsPersistenceTest,
	#superclass : #TestCase,
	#instVars : [
		'systemSettings',
		'memory',
		'preferencesFile'
	],
	#category : #'System-Settings-Tests'
}

{ #category : #asserting }
SystemSettingsPersistenceTest >> assertStoredSettingsDoNotInclude: aIdentifier [

	| nodes |
	self assert: preferencesFile exists.

	nodes := preferencesFile readStreamDo: [ :stream | STON fromStream: stream ].

	nodes detect: [ :aNode | aNode settingNodeIdentifier = aIdentifier printString ]
		ifFound: [ :aNode | self fail: 'The node has been found, it should not be there' ]
		ifNone: [ ^ self ]
]

{ #category : #asserting }
SystemSettingsPersistenceTest >> assertStoredSettingsInclude: aIdentifier withValue: aValue [

	| nodes |
	self assert: preferencesFile exists.

	nodes := preferencesFile readStreamDo: [ :stream | STON fromStream: stream ].

	nodes detect: [ :aNode | aNode settingNodeIdentifier = aIdentifier printString ]
		ifFound: [ :aNode | self assert: aNode realValue equals: aValue ]
		ifNone: [ self fail: 'Node with identifier ', aIdentifier printString, ' not found' ]
]

{ #category : #accessing }
SystemSettingsPersistenceTest >> booleanSettingNode [
	^ systemSettings nodeNamed: #booleanSetting
]

{ #category : #accessing }
SystemSettingsPersistenceTest >> rangeSettingNode [
	^ systemSettings nodeNamed: #rangeSetting
]

{ #category : #running }
SystemSettingsPersistenceTest >> setUp [
	super setUp.
	MockSettings cleanUp.
	memory := FileSystem memory.
	preferencesFile := memory root / 'pharo' / 'pharoX.Y' / 'preferences.ston.txt'.
	systemSettings := SystemSettingsPersistence new.
	systemSettings
		fileReference: preferencesFile;
		settingTree: MockSettings settingTree
]

{ #category : #accessing }
SystemSettingsPersistenceTest >> systemSettingNodeList [
	^ SettingBrowser new treeHolder nodeList
]

{ #category : #running }
SystemSettingsPersistenceTest >> tearDown [

	MockSettings cleanUp.
	super tearDown
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testAccessRealValues [
	"Ensure we can access all setting real values."
	self systemSettingNodeList do: [ :eachSettingNode |
				eachSettingNode item hasValue and: [ eachSettingNode item realValue ] ]
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testAccessibleRealValues [
	"Ensure we can access all setting real values."
	self systemSettingNodeList do: [ :eachSettingNode |
		eachSettingNode settingDeclaration hasValue and: [ eachSettingNode settingDeclaration realValue ] ]
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testAllStoredSettings [
	"There is no preference file. It should not generate an error."

	self assertEmpty: systemSettings allStoredSettings.
	systemSettings writeStream
		nextPutAll: 'random string';
		close.
	self assert: systemSettings fileReference contents equals: 'random string'.
	self assertEmpty: systemSettings allStoredSettings
]

{ #category : #'tests - storing - defaults' }
SystemSettingsPersistenceTest >> testDefaultImageDoesNotStoreAnySetting [
	| nodesToStore |
	"An image with all default values should not save any setting.
	The settings only should be saved if changed from default value"
	Author
		useAuthor: ''
		during: [
			nodesToStore :=  StoredSettingsFactory new fromSettingNodes:
				(MockSettings settingTree nodeList select: [ :aNode | aNode item hasDefault ]).
			nodesToStore := nodesToStore reject: [ :aNode | aNode hasDefaultValue ] ].

	self assertCollection: nodesToStore hasSameElements: #()
]

{ #category : #'tests - storing - defaults' }
SystemSettingsPersistenceTest >> testDefaultSettingValueIsNotIncludedInTheStoredFile [

	MockSettings booleanSettingWithDefault: false.

	systemSettings storeSettingNodes: MockSettings settingTree nodeList.

	self assertStoredSettingsDoNotInclude: #booleanSettingWithDefault
]

{ #category : #'tests - storing - defaults' }
SystemSettingsPersistenceTest >> testNonDefaultSettingValueIsIncludedInTheStoredFile [

	MockSettings booleanSettingWithDefault: true.

	systemSettings storeSettingNodes: MockSettings settingTree nodeList.

	self assertStoredSettingsInclude: #booleanSettingWithDefault withValue: true
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testRealValueModification [
	self booleanSettingNode realValue: false.
	self rangeSettingNode realValue: 55.
	self assert: self booleanSettingNode realValue equals: false.
	self assert: self rangeSettingNode realValue equals: 55
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testRealValues [
	self assert: (systemSettings nodeNamed: #booleanSetting) realValue equals: true.
	self assert: (systemSettings nodeNamed: #rangeSetting) realValue equals: 10
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testSettingNodeIdentifierForChildNode [
	self assert: MockSettings cacheDirectorySettingNode settingNodeIdentifier equals: '#monticello#cacheDirectory'
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testSettingNodeIdentifierForRootNode [
	self assert: MockSettings rangeSettingNode settingNodeIdentifier equals: '#rangeSetting'
]

{ #category : #'tests - loading' }
SystemSettingsPersistenceTest >> testStoreAndLoadAllSystemSettings [
	| searchCompiledMethod |
	systemSettings settingTree: SettingBrowser new treeHolder.
	systemSettings storeSettingNodes.
	systemSettings allStoredSettings do: #realValue.
	searchCompiledMethod := [ :aClass :aBlock | aClass compiledMethodAt: #= ifAbsent: [ aBlock value: aClass superclass value: aBlock ] ].
	systemSettings allStoredSettings do: [ :eachStoredSetting | | settingNode compiledMethod |
		settingNode := systemSettings settingNodeForStoredSetting: eachStoredSetting.
		self assert: eachStoredSetting realValue class equals: settingNode realValue class.
		compiledMethod := searchCompiledMethod value: eachStoredSetting realValue class value: searchCompiledMethod.
		compiledMethod methodClass == Object ifFalse: [
			self assert: eachStoredSetting realValue equals: settingNode realValue ] ]
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testStoreBooleanToSton [
	| node stored string stored2 |
	node := MockSettings booleanSettingNode.
	stored := StoredSettingsFactory new from: node.
	self assert: stored notNil.
	string := String streamContents: [ :aStream | | writer |
		writer := STON writer on: aStream; prettyPrint: true; yourself.
		writer writeObject: stored.
	].
	self assert: string notNil.
	stored2 := STON fromString: string.
	self assert: stored2 notNil.
	self assert: stored2 equals: stored
]

{ #category : #'tests - storing' }
SystemSettingsPersistenceTest >> testStoreIdentifier [
	self deny: preferencesFile exists.
	systemSettings storeIdentifier: MockSettings booleanSettingNode settingNodeIdentifier.
	self assert: preferencesFile exists.
	self assert: preferencesFile size > 0
]

{ #category : #'tests - storing' }
SystemSettingsPersistenceTest >> testStoreSettingNode [
	self deny: preferencesFile exists.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferencesFile exists.
	self assert: preferencesFile size > 0
]

{ #category : #'tests - storing' }
SystemSettingsPersistenceTest >> testStoreSettingNodeTwice [
	| fileSize fullPath |
	self deny: preferencesFile exists.
	fullPath := preferencesFile fullPath.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferencesFile fullPath equals: fullPath.
	self assert: preferencesFile exists.
	fileSize := preferencesFile size.
	self assert: fileSize > 0.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferencesFile fullPath equals: fullPath.
	self assert: preferencesFile size equals: fileSize
]

{ #category : #'tests - storing' }
SystemSettingsPersistenceTest >> testStoreSettingNodes [
	self deny: preferencesFile exists.
	self testRealValueModification.
	systemSettings storeSettingNodes: {
		MockSettings booleanSettingNode.
		MockSettings rangeSettingNode }.
	self assert: preferencesFile exists.
	self assert: preferencesFile size > 0.
	self assert: preferencesFile contents equals: MockSettings twoStoredSettingsSTONString
]

{ #category : #'tests - storing' }
SystemSettingsPersistenceTest >> testStoreTwoSettingNodeSeparately [
	| fileSize |
	self deny: preferencesFile exists.
	systemSettings storeSettingNode: MockSettings booleanSettingNode.
	self assert: preferencesFile exists.
	fileSize := preferencesFile size.
	self assert: fileSize > 0.
	systemSettings storeSettingNode: MockSettings rangeSettingNode.
	self assert: preferencesFile size > fileSize
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testStoredValueForSetting [
	| value |
	self testStoreSettingNode.
	value := systemSettings storedValueForSettingNode: MockSettings booleanSettingNode.
	self assert: value
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testStoredValueForSettingNoStoredValue [
	| value |
	value := systemSettings storedValueForSettingNode: MockSettings booleanSettingNode.
	self assert: value equals: nil
]

{ #category : #'tests - storing - defaults' }
SystemSettingsPersistenceTest >> testStoringNonDefaultValueAndThenStoringTheDefaultRemovesTheNode [

	"Store with a non default value"
	MockSettings booleanSettingWithDefault: true.
	systemSettings storeSettingNodes: MockSettings settingTree nodeList.

	"Store with a default value"
	MockSettings booleanSettingWithDefault: false.
	systemSettings storeSettingNodes: MockSettings settingTree nodeList.

	self assertStoredSettingsDoNotInclude: #booleanSettingWithDefault
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testUniqueIdentifiers [
	| names |
	names := self systemSettingNodeList collect: [:each | each settingNodeIdentifier].
	self assert: names size equals: names asSet size
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testUniqueNames [
	| names |
	names := self systemSettingNodeList collect: [ :eachNode |
						eachNode parentName printString, eachNode item name ].
	self assert: names size equals: names asSet size
]

{ #category : #'tests - loading' }
SystemSettingsPersistenceTest >> testUpdateAllSettings [
	self testRealValues.
	self testRealValueModification.
	systemSettings storeSettingNodes.
	MockSettings cleanUp.
	self testRealValues.
	systemSettings updateSettingNodes.
	self assert: self booleanSettingNode realValue equals: false.
	self assert: self rangeSettingNode realValue equals: 55
]

{ #category : #'tests - loading' }
SystemSettingsPersistenceTest >> testWriteAndReadBoolean [
	| node stored string stored2 |
	node := MockSettings booleanSettingNode.
	stored := StoredSettingsFactory new from: node.
	self assert: stored notNil.
	string := String streamContents: [ :aStream | | writer |
		writer := STON writer on: aStream; prettyPrint: true; yourself.
		writer writeObject: stored.
	].
	self assert: string notNil.
	stored2 := STON fromString: string.
	self assert: stored2 notNil.
	self assert: stored2 equals: stored
]

{ #category : #tests }
SystemSettingsPersistenceTest >> testWriteStream [
	| writeStream |
	self deny: preferencesFile exists.
	self deny: preferencesFile parent exists.
	writeStream := systemSettings writeStream.
	self assert: writeStream isStream.
	writeStream close
]

{ #category : #'tests - storing' }
SystemSettingsPersistenceTest >> testWritingStoredSettingNodesShouldNotCloseFileStream [
	| writeStream storedSettings |
	writeStream := preferencesFile ensureCreateFile writeStream.
	storedSettings := StoredSettingsFactory new
		fromSettingNodes: (Array with: MockSettings booleanSettingNode).
	systemSettings storeExactStoredSettings: storedSettings on: writeStream.
	self deny: writeStream closed
]
