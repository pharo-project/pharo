"
I represents Privacy settings. You can open Settings Browser and see Privacy group.

Execute: SettingBrowser open
"
Class {
	#name : #GTEventRecorderSettings,
	#superclass : #Object,
	#classInstVars : [
		'url'
	],
	#category : #'GT-EventRecorder-Settings'
}

{ #category : #cleanup }
GTEventRecorderSettings class >> cleanUp [
	"Clean settings."
	url := nil
]

{ #category : #accessing }
GTEventRecorderSettings class >> stringUrl [
	^ self url asString
]

{ #category : #accessing }
GTEventRecorderSettings class >> stringUrl: aUrlString [
	^ self url: aUrlString
]

{ #category : #accessing }
GTEventRecorderSettings class >> url [
	^ url ifNil: [ url := 'http://gc.dcc.uchile.cl.:8080/gt/events' asZnUrl ]
]

{ #category : #accessing }
GTEventRecorderSettings class >> url: aZnUrl [
	url := aZnUrl ifNotNil: [ aZnUrl asZnUrl ].
]

{ #category : #settings }
GTEventRecorderSettings class >> urlSettingsOn: aBuilder [ 
	<systemsettings>	
	(aBuilder setting: #stringUrl)
		parent: #gt;
		target: self;
		description: 'The default URL for the Event Recorder server.';
		label: 'Event Recorder URL address'

]
