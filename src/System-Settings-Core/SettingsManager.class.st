"
I manage a set of settings. I can load them from disk, store them to disk.
"
Class {
	#name : 'SettingsManager',
	#superclass : 'Object',
	#instVars : [
		'storedSettingsReader',
		'readStream',
		'writeStream',
		'settings',
		'storedSettingsMerger',
		'storedSettingsWriter',
		'settingsFile'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'accessing' }
SettingsManager class >> defaultPreferencesFileReference [

	^ StartupPreferencesLoader preferencesVersionFolder / 'system-settings.ston'
]

{ #category : 'instance creation' }
SettingsManager class >> forPragmaNamed: aSymbol [

	^ self forSettings: (SettingsBuilder forPragmaNamed: aSymbol) settings
]

{ #category : 'instance creation' }
SettingsManager class >> forSettings: aListOfSettings [

	^ self new
		settings: aListOfSettings;
		yourself 
]

{ #category : 'loading' }
SettingsManager >> allStoredSettings [
	"Load all stored settings from the read stream."
	
	^ self storedSettingsReader secureLoad
]

{ #category : 'accessing' }
SettingsManager >> defaultStoredSettingsMerger [

	^ StoredSettingsMerger new
]

{ #category : 'accessing' }
SettingsManager >> defaultStoredSettingsReader [

	^ SettingsStonReader new
]

{ #category : 'accessing' }
SettingsManager >> defaultStoredSettingsWriter [

	^ SettingsStonWriter new
]

{ #category : 'private - storing' }
SettingsManager >> ensureSettingsFileParent [
	"It ensures that the parent directory exists."

	self settingsFile ifNotNil: [ :file |
		file parent ensureCreateDirectory ]
]

{ #category : 'loading' }
SettingsManager >> loadSettings [
	
	self updateSettingsWith: self allStoredSettings
]

{ #category : 'private - storing' }
SettingsManager >> mergeStoredSettings: aCollectionOfStoredSettings [

	^ self storedSettingsMerger
		addStoredSettings: self allStoredSettings;
		addStoredSettings: aCollectionOfStoredSettings;
		storedSettings
]

{ #category : 'accessing' }
SettingsManager >> readStream [

	^ readStream ifNil: [
			  self settingsFile ifExists: [ :file | file readStream ] ifAbsent: [ nil ] ]
]

{ #category : 'accessing' }
SettingsManager >> readStream: aReadStream [ 
	readStream := aReadStream
]

{ #category : 'private - storing' }
SettingsManager >> removeSettingsFileIfExists [
	"We have to remove the file before writing new settings."

	| newFileReference |
	settingsFile ifNil: [ ^ self ].
	settingsFile ifAbsent: [ ^ self ].
	
	newFileReference := self settingsFile withExtension: 'old.txt'.
	newFileReference ensureDelete.
	self settingsFile copy renameTo: newFileReference basename
]

{ #category : 'accessing' }
SettingsManager >> settingNamed: aName [

	^ settings detect: [ :setting | setting name = aName ]
]

{ #category : 'accessing' }
SettingsManager >> settings [

	^ settings
]

{ #category : 'accessing' }
SettingsManager >> settings: aListOfSettings [

	settings := aListOfSettings
]

{ #category : 'accessing' }
SettingsManager >> settingsFile [
	^ settingsFile ifNotNil: [ settingsFile asFileReference ]
]

{ #category : 'accessing' }
SettingsManager >> settingsFile: aFileLocator [
	"We use a File Locator to ensure the preferences folder is updated even if the image is moved in a new machine"
	settingsFile := aFileLocator 
]

{ #category : 'accessing' }
SettingsManager >> settingsFromIdentifiers: identifiers [

	^ identifiers
		  collect: [ :identifier |
				  self settings detect: [ :setting |
					  setting settingIdentifier = identifier ] ]
		  thenReject: [ :each | each isNil ]
]

{ #category : 'storing' }
SettingsManager >> storeExactStoredSettings: allStoredSettings [

	self
		ensureSettingsFileParent;
		removeSettingsFileIfExists.
		
	self settingsFile writeStreamDo: [ :stream |
		self storeExactStoredSettings: allStoredSettings on: stream ]
]

{ #category : 'private - storing' }
SettingsManager >> storeExactStoredSettings: aCollectionOfStoredSettings on: aStream [

	self storedSettingsWriter
		stream: aStream;
		addSettings: aCollectionOfStoredSettings;
		store
]

{ #category : 'storing' }
SettingsManager >> storeIdentifier: aSettingIdentifier [
	
	self storeIdentifiers: (Array with: aSettingIdentifier)
]

{ #category : 'storing' }
SettingsManager >> storeIdentifiers: aListOfSettingIdentifiers [
	
	settings := self settingsFromIdentifiers: aListOfSettingIdentifiers.
	self storeSettings: settings 
]

{ #category : 'storing' }
SettingsManager >> storeSetting: aSetting [
	
	self storeSettings: (Array with: aSetting)
]

{ #category : 'storing' }
SettingsManager >> storeSettings [
	
	settingsFile ifNil: [ ^ self ].
	self storeSettings: self settings
]

{ #category : 'storing' }
SettingsManager >> storeSettings: aListOfSettings [
	
	self storeStoredSettings: (self storedSettingsFor: aListOfSettings)
]

{ #category : 'storing' }
SettingsManager >> storeStoredSettings: aCollectionOfStoredSettings [
	"It stores a collection of stored settings on the disk."

	| allStoredSettings |
	allStoredSettings := self mergeStoredSettings: aCollectionOfStoredSettings.
	self storeExactStoredSettings: allStoredSettings
]

{ #category : 'accessing' }
SettingsManager >> storedSettingFactory [

	^ StoredSettingsFactory new
]

{ #category : 'loading' }
SettingsManager >> storedSettingFor: aSettingIdentifier [

	^ self allStoredSettings
				detect: [ :eachStoredSetting |
					eachStoredSetting isForSettingIdentifier: aSettingIdentifier ]
]

{ #category : 'converting' }
SettingsManager >> storedSettingsFor: aListOfSetting [

	^ self storedSettingFactory fromSettingNodes: aListOfSetting
]

{ #category : 'accessing' }
SettingsManager >> storedSettingsMerger [

	^ storedSettingsMerger ifNil: [ self defaultStoredSettingsMerger ]
]

{ #category : 'accessing' }
SettingsManager >> storedSettingsReader [

	^ (storedSettingsReader ifNil: [ self defaultStoredSettingsReader ])
		stream: self readStream;
		yourself
]

{ #category : 'accessing' }
SettingsManager >> storedSettingsReader: aStoredSettingReader [

	storedSettingsReader := aStoredSettingReader
]

{ #category : 'accessing' }
SettingsManager >> storedSettingsWriter [

	^ storedSettingsWriter ifNil: [ self defaultStoredSettingsWriter ]
]

{ #category : 'loading' }
SettingsManager >> updateSettings: someSettings [

	self updateSettings: someSettings with: self allStoredSettings
]

{ #category : 'private - loading' }
SettingsManager >> updateSettings: aLitOfSettings with: aListOfStoredSettings [

	aListOfStoredSettings do: [ :storedSetting |
			aLitOfSettings
				detect: [ :aSetting | storedSetting isForSettingIdentifier: aSetting settingIdentifier ]
				ifFound: [ :aSetting | storedSetting updateSettingNode: aSetting ]
				ifNone: [ "ignore it" ] ]
]

{ #category : 'private - loading' }
SettingsManager >> updateSettingsWith: aListOfStoredSettings [

	self updateSettings: settings with: aListOfStoredSettings
]

{ #category : 'configuring' }
SettingsManager >> useDefaultPreferencesFile [

	self settingsFile: self class defaultPreferencesFileReference
]

{ #category : 'private - storing' }
SettingsManager >> writeStream [

	^ writeStream ifNil: [
			  self ensureSettingsFileParent.
			  self removeSettingsFileIfExists.
			  self settingsFile writeStream ]
]

{ #category : 'private - storing' }
SettingsManager >> writeStreamDo: aBlock [
	| stream |
	stream := self writeStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
]
