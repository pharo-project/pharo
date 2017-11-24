"
I merge several stored settings together. If you ask me twice to store same stored settings, the last one wins. 

Collaborators: SystemSettingsPersistence

Public API and Key Messages

- addStoredSetting:
- addStoredSettings:
- storedSettings

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	storedSettings:		<Collection of StoredSetting>

"
Class {
	#name : #StoredSettingsMerger,
	#superclass : #Object,
	#instVars : [
		'storedSettings'
	],
	#category : #'System-Settings-Persistence-Ston-StoredSettings-Builder'
}

{ #category : #adding }
StoredSettingsMerger >> addStoredSetting: aSettingNode [ 
	self storedSettings withIndexDo: [ :eachStoredNode :index | 
		eachStoredNode = aSettingNode ifTrue: [ ^ self storedSettings at: index put: aSettingNode ] ].
	^ self storedSettings add: aSettingNode
]

{ #category : #adding }
StoredSettingsMerger >> addStoredSettings: aCollectionOfStoredSettings [
	aCollectionOfStoredSettings do: [ :eachStoredSetting |
		self addStoredSetting: eachStoredSetting ].
	^ aCollectionOfStoredSettings
]

{ #category : #initialization }
StoredSettingsMerger >> initialize [
	super initialize.
	storedSettings := OrderedCollection new.
]

{ #category : #accessing }
StoredSettingsMerger >> storedSettings [
	^ storedSettings
]
