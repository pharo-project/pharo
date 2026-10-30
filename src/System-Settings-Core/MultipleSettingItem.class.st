"
I represent an item of a setting that can have multiple values.
I have a label and a value.
"
Class {
	#name : 'MultipleSettingItem',
	#superclass : 'Object',
	#instVars : [
		'name',
		'value',
		'setting'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'instance creation' }
MultipleSettingItem class >> newFrom: associationOrString setting: aSetting [
	associationOrString isAssociation 
		ifTrue: [ ^ self newFromAssociation: associationOrString setting: aSetting ].
	associationOrString isString
		ifTrue: [ self newFromString: associationOrString asString setting: aSetting ].
	^ self newFromObject: associationOrString setting: aSetting
]

{ #category : 'instance creation' }
MultipleSettingItem class >> newFromAssociation: anAssociation setting: aSetting [

	^ self new
		name: anAssociation key;
		value: anAssociation value;
		setting: aSetting;
		yourself
]

{ #category : 'instance creation' }
MultipleSettingItem class >> newFromObject: anObject setting: aSetting [

	^ self 
		newFromAssociation: anObject asString -> anObject
		setting: aSetting 
]

{ #category : 'instance creation' }
MultipleSettingItem class >> newFromString: aString setting: aSetting [

	^ self new
		name: aString;
		value: aString;
		setting: aSetting;
		yourself
]

{ #category : 'updating' }
MultipleSettingItem >> beCurrentValue [

	setting realValue: self value
]

{ #category : 'accessing' }
MultipleSettingItem >> name [

	^ name
]

{ #category : 'accessing' }
MultipleSettingItem >> name: aString [

	name := aString
]

{ #category : 'printing' }
MultipleSettingItem >> printOn: aStream [

	super printOn: aStream.
	aStream 
		nextPut: Character space;
		nextPutAll: name;
		nextPutAll: '->'.
	value printOn: aStream
	
]

{ #category : 'accessing' }
MultipleSettingItem >> setting [

	^ setting
]

{ #category : 'accessing' }
MultipleSettingItem >> setting: aSetting [

	setting := aSetting
]

{ #category : 'accessing' }
MultipleSettingItem >> value [

	^ value
]

{ #category : 'accessing' }
MultipleSettingItem >> value: anObject [

	value := anObject
]
