"
I contains some system settings that are used for testing.
"
Class {
	#name : #MockSettings,
	#superclass : #Object,
	#classInstVars : [
		'cacheDirectory',
		'booleanMockSetting',
		'rangeSetting',
		'defaultDirectoryName'
	],
	#category : #System-Settings-Tests
}

{ #category : #accessing }
MockSettings class >> booleanSetting [
	^ booleanMockSetting ifNil: [ booleanMockSetting := true ]
]

{ #category : #accessing }
MockSettings class >> booleanSetting: aBoolean [
	^ booleanMockSetting := aBoolean
]

{ #category : #settings }
MockSettings class >> booleanSettingNode [
	^ self settingTree nodeNamed: #booleanSetting
]

{ #category : #settings }
MockSettings class >> booleanSettingNodeOn: aBuilder [
	<mocksystemsettings>
	(aBuilder setting: #booleanSetting)
		label: 'Mock boolean setting'.
]

{ #category : #accessing }
MockSettings class >> cacheDirectory [
	^ cacheDirectory ifNil: [ cacheDirectory := FileLocator workingDirectory ]
]

{ #category : #accessing }
MockSettings class >> cacheDirectory: aFileReference [
	cacheDirectory := aFileReference
]

{ #category : #settings }
MockSettings class >> cacheDirectorySettingNode [
	^ self settingTree nodeNamed: #cacheDirectory
]

{ #category : #'setting strings' }
MockSettings class >> changedClassStoredSettingVariable [
	^ 
'[
	ClassStoredSetting {
		#notExistingInstanceVariable : nil
	},
	ClassStoredSetting {
		#settingNodeIdentifier : ''#notExistingClassStoredSettingSTONString'',
		#realValue : #MockNotExistingClass
	}
]'
]

{ #category : #cleanup }
MockSettings class >> cleanUp [
	self booleanSetting: nil.
	self rangeSetting: nil.
	self defaultDirectoryName: nil.
	self cacheDirectory: nil.
]

{ #category : #accessing }
MockSettings class >> defaultDirectoryName [
	^ defaultDirectoryName ifNil: [ defaultDirectoryName := FileLocator workingDirectory ]
]

{ #category : #accessing }
MockSettings class >> defaultDirectoryName: aFileReference [
	defaultDirectoryName := aFileReference
]

{ #category : #settings }
MockSettings class >> monticelloSettingsOn: aBuilder [  
	<mocksystemsettings>  
	(aBuilder group: #monticello)
		label: 'Mock Monticello';
		description: 'All Mock Monticello settings';
		with: [ 
			(aBuilder setting: #defaultDirectoryName)
				type: #Directory;
				target: self;
				description: 'The path of a directory where you want to start out when choosing local repository locations' ;
				label: 'Default local repository directory'.
			(aBuilder setting: #cacheDirectory)
				type: #Directory;
				target: self;
				description: 'The path of the local repository cache';
				label: 'Local cache directory'.	
			].
]

{ #category : #accessing }
MockSettings class >> nodeSettings [
	"self nodeSettings"
	^ self settingTree nodeList
]

{ #category : #'setting strings' }
MockSettings class >> notExistingClassStoredSettingSTONString [
	^ 
'[
	ClassStoredSetting {
		#settingNodeIdentifier : ''#notExistingClassStoredSettingSTONString'',
		#realValue : #MockNotExistingClass
	}
]'
]

{ #category : #'setting strings' }
MockSettings class >> oneStoredSettingSTONString [
	^ 
'[
	StoredSetting {
		#settingNodeIdentifier : ''#booleanSetting'',
		#realValue : true
	}
]'
]

{ #category : #accessing }
MockSettings class >> rangeSetting [
	^ rangeSetting ifNil: [ rangeSetting := 10 ].
]

{ #category : #accessing }
MockSettings class >> rangeSetting: anInteger [ 
	rangeSetting := anInteger
]

{ #category : #settings }
MockSettings class >> rangeSettingNode [
	^ self settingTree nodeNamed: #rangeSetting
]

{ #category : #settings }
MockSettings class >> rangeSettingNodeOn: aBuilder [
	<mocksystemsettings>
	(aBuilder range: #rangeSetting)
		label: 'Mock range setting';
		range: (0 to: 100 by: 5)
]

{ #category : #accessing }
MockSettings class >> settingTree [
	"self settingTree"
	^ SettingTree new acceptableKeywords: (Array with: #mocksystemsettings)
]

{ #category : #accessing }
MockSettings class >> storedSettings [
	"self storedSettings"
	| builder |
	builder := StoredSettingsFactory new.
	^ self nodeSettings collect: [ :e | builder from: e ]
]

{ #category : #'setting strings' }
MockSettings class >> twoStoredSettingsSTONString [
	^ 
'[
	StoredSetting {
		#settingNodeIdentifier : ''#booleanSetting'',
		#realValue : false
	},
	StoredSetting {
		#settingNodeIdentifier : ''#rangeSetting'',
		#realValue : 55
	}
]'
]
