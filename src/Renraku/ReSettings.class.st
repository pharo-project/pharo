"
i have settings for QA recording functions
"
Class {
	#name : #ReSettings,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Renraku-Utility'
}

{ #category : #cleanup }
ReSettings class >> cleanUp [
	
	uniqueInstance := nil
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

{ #category : #accessing }
ReSettings class >> uniqueInstance [
	
	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]
