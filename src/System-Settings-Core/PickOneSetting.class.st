"
I represent a setting for which the value is a value from the list of domain values.
If domainValues is set, then the list of valid values is constant (initialized at declaration time). Instead, if getter is set, then the setting list is always dynamically computed.
"
Class {
	#name : 'PickOneSetting',
	#superclass : 'Setting',
	#instVars : [
		'domainValues'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'accessing' }
PickOneSetting >> domainValues [
	^ domainValues
]

{ #category : 'accessing' }
PickOneSetting >> domainValues: aCollection [ 

	domainValues := aCollection collect: [ :each | MultipleSettingItem newFrom: each setting: self] as: Array
]
