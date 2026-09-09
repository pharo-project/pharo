"
A RangeSettingValue is a setting for which the value domain is an interval.  See SettingManager comment for more explanations.

Instance Variables
	range:		<Interval>

range
	- The interval which define the value domain

"
Class {
	#name : 'RangeSettingDeclaration',
	#superclass : 'SettingDeclaration',
	#instVars : [
		'range'
	],
	#category : 'System-Settings-Core-Base',
	#package : 'System-Settings-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
RangeSettingDeclaration >> range [
	^ range
]

{ #category : 'accessing' }
RangeSettingDeclaration >> range: anInterval [
	range := anInterval
]

{ #category : 'accessing' }
RangeSettingDeclaration >> realValue: aValue [
	aValue
		ifNil: [self settingReceiver perform: self setSelector with: nil]
		ifNotNil: [self settingReceiver perform: self setSelector with: ((aValue max: self range first) min: self range last)].
	self triggerEvent: self privateChangeEvent with: self.
	^ true
]
