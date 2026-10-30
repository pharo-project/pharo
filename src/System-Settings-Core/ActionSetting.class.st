"
I represent an action modeled as a setting.
It is a convenience to build a UI automatically but should maybe not be used.
"
Class {
	#name : 'ActionSetting',
	#superclass : 'AbstractSetting',
	#instVars : [
		'actionLabel'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'accessing' }
ActionSetting >> buttonLabel [

	^ actionLabel
]

{ #category : 'accessing' }
ActionSetting >> buttonLabel: aString [

	actionLabel := aString
]

{ #category : 'setting - polymorphism' }
ActionSetting >> hasDefaultValue [

	"Do not store this setting. It has no value"
	^ true
]

{ #category : 'setting - polymorphism' }
ActionSetting >> realValue [

	"An action has no value"
	^ nil
]
