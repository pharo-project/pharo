"
When defining a description of a setting, you don't need to translate it, and this will be done by the setting itself.
"
Class {
	#name : #SettingDontTranslateDescriptionRule,
	#superclass : #ReNodeRewriteRule,
	#category : #System-Settings-Rules
}

{ #category : #running }
SettingDontTranslateDescriptionRule >> check: aNode forCritiquesDo: aBlock [

	aNode methodNode ifNotNil: [ :methNode |
		(self checkPreconditionOn: methNode) ifTrue: [
			super check: aNode forCritiquesDo: aBlock ] ]
]

{ #category : #helpers }
SettingDontTranslateDescriptionRule >> checkPreconditionOn: aMethodNode [

	^ aMethodNode pragmas anySatisfy: [ :p |
		p selector = #systemsettings ]
]

{ #category : #accessing }
SettingDontTranslateDescriptionRule >> group [

	^ 'API Hints'
]

{ #category : #initialization }
SettingDontTranslateDescriptionRule >> initialize [
	super initialize.
	
	self
		replace: '`@recv description: `#str translated'
		with: '`@recv description: `#str'
]

{ #category : #accessing }
SettingDontTranslateDescriptionRule >> name [

	^ 'Do not use #translated for setting''s description'
]
