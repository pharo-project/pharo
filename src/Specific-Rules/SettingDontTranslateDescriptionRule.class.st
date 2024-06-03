"
When defining a description of a setting, you don't need to translate it. It will be done by the setting itself.
"
Class {
	#name : 'SettingDontTranslateDescriptionRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'Specific-Rules-API Hints',
	#package : 'Specific-Rules',
	#tag : 'API Hints'
}

{ #category : 'accessing' }
SettingDontTranslateDescriptionRule class >> group [

	^ 'API Hints'
]

{ #category : 'accessing' }
SettingDontTranslateDescriptionRule class >> ruleName [
	^ 'Do not use #translated for setting''s description'
]

{ #category : 'running' }
SettingDontTranslateDescriptionRule >> basicCheck: aNode [
	^ aNode methodNode ifNil: [ false ] ifNotNil: [ :methNode | self checkPreconditionOn: methNode ]
]

{ #category : 'helpers' }
SettingDontTranslateDescriptionRule >> checkPreconditionOn: aMethodNode [

	^ aMethodNode pragmas anySatisfy: [ :p |
		p selector = #systemsettings ]
]

{ #category : 'initialization' }
SettingDontTranslateDescriptionRule >> initialize [
	super initialize.

	self
		replace: '`@recv description: `#str translated'
		with: '`@recv description: `#str'
]
