"
I am a reader of system settings from a file (stream). I know how to load the settings and I return those stored settings.

Collaborators: SystemSettingsPersistence

Public API and Key Messages

- stream:   
- secureLoad 
- load

Internal Representation and Key Implementation Points.

    Instance Variables
	storedSettings:		<Array>
	stream:				<ReadStream>

"
Class {
	#name : #SettingsStonReader,
	#superclass : #Object,
	#instVars : [
		'stream',
		'storedSettings'
	],
	#category : #System-Settings-Persistence-Ston-StoredSettings-Builder
}

{ #category : #loading }
SettingsStonReader >> load [
	"It loads all instances of StoredSetting from a stream."
	stream ifNil: [ ^ #() ].
	stream atEnd ifTrue: [ ^ #() ].
	[ | reader | 
		reader := STON reader on: stream; yourself.
		storedSettings := reader next.
	] ensure: [ stream close ].
	^ storedSettings
]

{ #category : #loading }
SettingsStonReader >> secureLoad [
	[ self load ] on: STONReaderError do: [ :ex | "ignore" ].
	^ self storedSettings ifNil: [ #() ].
]

{ #category : #accessing }
SettingsStonReader >> storedSettings [
	^ storedSettings
]

{ #category : #accessing }
SettingsStonReader >> stream [
	^ stream
]

{ #category : #accessing }
SettingsStonReader >> stream: anObject [
	stream := anObject
]
