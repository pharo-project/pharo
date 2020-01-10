"
i have settings for QA recording functions
"
Class {
	#name : #ReSettings,
	#superclass : #Object,
	#instVars : [
		'inspectorPlugin',
		'spotterPlugin'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Renraku-Utility'
}

{ #category : #cleanup }
ReSettings class >> cleanUp [
	
	
	uniqueInstance := nil
]

{ #category : #'settings-accessing' }
ReSettings class >> inspectorPluggin [
	
	^ self uniqueInstance inspectorPluggin
]

{ #category : #'settings-accessing' }
ReSettings class >> inspectorPluggin: aBoolean [ 
	
	self uniqueInstance inspectorPluggin: aBoolean
]

{ #category : #'settings-definition' }
ReSettings class >> inspectorPlugginSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #inspectorPluggin)
		order: 5; 
		target: self;
		default: false;
		label: 'Inspector Plugin';
		description: 'Enable GTInspector plugin which shows a tab with number of critiques and allows to inspect them.';
		parent: #qualityAssistant.
]

{ #category : #'settings-definition' }
ReSettings class >> resetButtonSettingsOn: aBuilder [
	<systemsettings>
		
	(aBuilder group: #'resetRules')
		order: 10;
		target: self;
		label: 'Rule cache';
		parent: #qualityAssistant;
		description: 'Renraku (the engine behind quality rules) caches the rule objects for performance boost. Here you can reset the cache, this may be useful if you''ve implemented a new rule and want to include it.';
		dialog: [
			SimpleButtonMorph new
				target: ReRuleManager;
				label: 'Reset rule cache';
				actionSelector: #resetWithInform;
				themeChanged;
				yourself ].
]

{ #category : #'settings-accessing' }
ReSettings class >> showErrors [
	self flag: 'switch to exception strategy completely'.
	"Utilize the ReExceptionStrategy to either skip the exception,
	pass it on, or create a list of exceptions as a reparete
	'category' with all the exceptions that happened."
	
	^ ReExceptionStrategy current = RePassExceptionStrategy
]

{ #category : #'settings-accessing' }
ReSettings class >> showErrors: aBoolean [

	ReExceptionStrategy current: (aBoolean 
		ifTrue: [ RePassExceptionStrategy ]
		ifFalse: [ ReIgnoreExceptionStrategy ]).
]

{ #category : #'settings-definition' }
ReSettings class >> showErrorsSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #showErrors)
		target: self;
		order: 5;
		default: false;
		label: 'Show errors (dev. only!)';
		description: 'Signal errors that happen during critique-checking (and related). This should be used by developers.';
		parent: #qualityAssistant.
]

{ #category : #'settings-accessing' }
ReSettings class >> spotterPlugin [
	
	^ self uniqueInstance spotterPlugin
]

{ #category : #'settings-accessing' }
ReSettings class >> spotterPlugin: aBoolean [
	
	^ self uniqueInstance spotterPlugin: aBoolean
]

{ #category : #'settings-definition' }
ReSettings class >> spotterPluginSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #spotterPlugin)
		order: 5;
		target: self;
		label: 'Spotter Plugin';
		description: 'Enable GTSpotter plugin which shows a group with critiques and allows to preview them.';
		default: false;
		parent: #qualityAssistant.
]

{ #category : #accessing }
ReSettings class >> uniqueInstance [
	
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : #initialization }
ReSettings >> initialize [
	super initialize.
	
	inspectorPlugin := false.
	spotterPlugin := false
]

{ #category : #accessing }
ReSettings >> inspectorPluggin [
	

	^ inspectorPlugin
]

{ #category : #accessing }
ReSettings >> inspectorPluggin: aBool [ 
	
	inspectorPlugin := aBool
]

{ #category : #accessing }
ReSettings >> spotterPlugin [
	
	^ spotterPlugin

]

{ #category : #accessing }
ReSettings >> spotterPlugin: aBool [ 
	
	spotterPlugin := aBool

]
