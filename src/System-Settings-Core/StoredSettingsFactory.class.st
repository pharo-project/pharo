"
I am responsible for creating new StoredSetting objects from existing SettingNodes. I know how to create a StoredSetting from a SettingNode.

Collaborators: SystemSettingsPersistence

Public API and Key Messages

- from:
- fromSettingNodes:
 
Internal Representation and Key Implementation Points.

    Instance Variables
	storedSetting:		<StoredSettings>
	storedSettings:		<Collection of StoredSettings>

"
Class {
	#name : #StoredSettingsFactory,
	#superclass : #Object,
	#instVars : [
		'storedSetting',
		'storedSettings'
	],
	#category : #'System-Settings-Core-Persistence-Ston-StoredSettings-Builder'
}

{ #category : #building }
StoredSettingsFactory >> from: aSettingNode [ 
	storedSetting := nil.
	aSettingNode item acceptSettings: self. 
	storedSetting ifNotNil: [ 
		storedSetting settingNodeIdentifier: aSettingNode settingNodeIdentifier ].
	^ storedSetting
]

{ #category : #building }
StoredSettingsFactory >> fromSettingNodes: aCollectionOfSettingNodes [ 
	storedSettings := OrderedCollection new.
	
	aCollectionOfSettingNodes 
		select: [ :eachSettingNode | 
			eachSettingNode item hasValue and: [ eachSettingNode item hasDefaultValue not ] ] 
		thenDo: [ :eachSettingNode |
			(self from: eachSettingNode) ifNotNil: [ :aStoredNode | storedSettings add: aStoredNode ] ].
	
	^ storedSettings
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newAbsolutePathStoredSetting [
	^ storedSetting := AbsolutePathStoredSetting new
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newClassStoredSetting [
	^ storedSetting := ClassStoredSetting new.
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newFileLocatorStoreSetting [
	^ storedSetting := FileLocatorStoredSetting new
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newFileReferenceStoredSetting [
	^ storedSetting := AbsolutePathStoredSetting new
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newLogicalFontStoredSetting [
	^ storedSetting := LogicalFontStoredSetting new
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newRelativePathStoredSetting [
	^ storedSetting := RelativePathStoredSetting new
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newStoredSetting [
	^ storedSetting := StoredSetting new.
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newStrikeFontStoredSetting [
	^ storedSetting := StrikeFontStoredSetting new
]

{ #category : #'instance creation' }
StoredSettingsFactory >> newThemeIconsStoredSetting [
	^ storedSetting := ThemeIconsStoredSetting new
]

{ #category : #visitor }
StoredSettingsFactory >> visitAbsolutePath: anAbsolutePath [ 
	self newAbsolutePathStoredSetting realValue: anAbsolutePath segments
]

{ #category : #visitor }
StoredSettingsFactory >> visitAbstractFileReference: aFileReference [ 
	aFileReference path acceptSettings: self.
]

{ #category : #visitor }
StoredSettingsFactory >> visitClass: aClass [ 
	self newClassStoredSetting realValue: aClass name.
]

{ #category : #visitor }
StoredSettingsFactory >> visitFileLocator: aFileLocator [ 
	self newFileLocatorStoreSetting 
			origin: aFileLocator origin;
			pathSegments: aFileLocator path segments 
]

{ #category : #visitor }
StoredSettingsFactory >> visitLogicalFont: aLogicalFont [ 
	self newLogicalFontStoredSetting
		fontClassName: aLogicalFont class name;
		familyName: aLogicalFont familyName;
		fallbackFamilyNames: aLogicalFont fallbackFamilyNames;
		pointSize: aLogicalFont pointSize;
		stretchValue: aLogicalFont stretchValue;
		weightValue: aLogicalFont weightValue;
		slantValue: aLogicalFont slantValue.
]

{ #category : #visitor }
StoredSettingsFactory >> visitObject: anObject [ 
	self newStoredSetting realValue: anObject
]

{ #category : #visitor }
StoredSettingsFactory >> visitPragmaSetting: aPragmaSetting [ 
	"do nothing"
]

{ #category : #visitor }
StoredSettingsFactory >> visitRelativePath: aRelativePath [ 
	self newRelativePathStoredSetting realValue: aRelativePath segments
]

{ #category : #visitor }
StoredSettingsFactory >> visitSettingDeclaration: aSettingDeclaration [ 
	aSettingDeclaration realValue acceptSettings: self.
	
]

{ #category : #visitor }
StoredSettingsFactory >> visitStrikeFont: aStrikeFont [ 
	self newStrikeFontStoredSetting 
			fontClassName: aStrikeFont class name;
			familyName: aStrikeFont familyName;
			pointSize: aStrikeFont pointSize;
			emphasized: aStrikeFont emphasis
]

{ #category : #visitor }
StoredSettingsFactory >> visitThemeIcons: aThemeIcons [ 
	self newThemeIconsStoredSetting 
			name: aThemeIcons name;
			url: aThemeIcons url
]
