"
I represent a part of a setting that can be stored to a preference file.

Responsibility: I keep a setting's stored value and I know to which setting I belong.

Collaborators: 
 - SettingsManager stores and loads my instances, and updates the settings with their stored values.
 - StoredSettingsFactory creates instances of me.
 - I represent stored value of #realValue of a setting.

Public API and Key Messages
- realValue returns the setting's stored value.
- isForSettingIdentifier: returns true if my instance belongs to the setting with the given identifier.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodClass:		<Class>
	selector:			<Symbol>
	realValue:		<Object>

"
Class {
	#name : 'StoredSetting',
	#superclass : 'AbstractStoredSetting',
	#instVars : [
		'realValue'
	],
	#category : 'System-Settings-Core-Persistence-Ston-StoredSettings',
	#package : 'System-Settings-Core',
	#tag : 'Persistence-Ston-StoredSettings'
}

{ #category : 'accessing' }
StoredSetting >> realValue [
	^ realValue
]

{ #category : 'accessing' }
StoredSetting >> realValue: anObject [
	realValue := anObject
]
