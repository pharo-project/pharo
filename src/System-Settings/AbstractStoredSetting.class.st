"
I am an abstract class of all stored settings. My subclasses are responsible for keeping SettingNode identifier and its real value that is supposed to store or has been loaded. There are some objects that cannot by directly stored into a file, e.g. fonts, because they includes too much information for storing. For that reason SettingNode objects are converted into objects of my subclasses that keeps storing simple. 

Collaborators: StoredSettingsFactory, SystemSettingsPersistence

Public API and Key Messages

- realValue
- settingNodeIdentifier 
- settingNodeIdentifier:
- isForSettingNode:
- StoredSettingsFactory creates instances of me.

Internal Representation and Key Implementation Points.

    Instance Variables
	settingNodeIdentifier:		<Symbol>

"
Class {
	#name : #AbstractStoredSetting,
	#superclass : #Object,
	#instVars : [
		'settingNodeIdentifier'
	],
	#category : #System-Settings-Persistence-Ston-StoredSettings
}

{ #category : #comparing }
AbstractStoredSetting >> = aStoredSetting [
	^ self class = aStoredSetting class and: [ 
		self settingNodeIdentifier = aStoredSetting settingNodeIdentifier ]
]

{ #category : #comparing }
AbstractStoredSetting >> hash [
	^ self class hash bitXor: self settingNodeIdentifier hash
]

{ #category : #testing }
AbstractStoredSetting >> isForSettingNode: aSettingNode [ 
	^ self settingNodeIdentifier = aSettingNode settingNodeIdentifier
]

{ #category : #printing }
AbstractStoredSetting >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		print: self settingNodeIdentifier;
		nextPut: $)
]

{ #category : #accessing }
AbstractStoredSetting >> realValue [
	^ self subclassResponsibility 
]

{ #category : #accessing }
AbstractStoredSetting >> settingNodeIdentifier [
	^ settingNodeIdentifier
]

{ #category : #accessing }
AbstractStoredSetting >> settingNodeIdentifier: aString [
	settingNodeIdentifier := aString
]

{ #category : #loading }
AbstractStoredSetting >> updateSettingNode: aSettingNode [ 
	| value |
	[ value := self realValue ] on: Error do: [ :exception | "ignore and leave" 
		self inform: 'Cannot read stored value of ', self settingNodeIdentifier, '. Exception: ', exception printString.
		^ self ].
	[ aSettingNode realValue: value ] on: Error do: [ :exception | "inform and ignore"
		self inform: 'Cannot update ', self settingNodeIdentifier, '. Exception: ', exception printString ]
]
