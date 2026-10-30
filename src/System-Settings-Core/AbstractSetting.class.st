"
I factor common slots between a Setting and a Setting Category.

Setting name must be unique among all settings managed by the same SettingManager.

A setting may have a parent. A setting's parent is always a SettingGroup.
"
Class {
	#name : 'AbstractSetting',
	#superclass : 'Object',
	#instVars : [
		'name',
		'label',
		'description',
		'order',
		'target',
		'targetSelector',
		'defaultValue',
		'declaration',
		'writeBlock',
		'readBlock',
		'parentName',
		'parent',
		'isTransient'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'comparing' }
AbstractSetting >> = other [

	^ self species = other species 
		and: [ self name = other name ]
]

{ #category : 'visiting' }
AbstractSetting >> acceptSettings: aVisitor [
	^ aVisitor visitSettingDeclaration: self
]

{ #category : 'private' }
AbstractSetting >> builder [

	^ ActiveSettingBuilder value
]

{ #category : 'accessing' }
AbstractSetting >> declaration [
	
	^ declaration
]

{ #category : 'accessing' }
AbstractSetting >> declaration: aPragma [
	
	declaration := aPragma
]

{ #category : 'accessing' }
AbstractSetting >> defaultTarget [

	"Return the class where the setting was defined"
	^ self declaration methodClass instanceSide
]

{ #category : 'accessing' }
AbstractSetting >> defaultValue [

	^ defaultValue
]

{ #category : 'accessing' }
AbstractSetting >> defaultValue: aValue [

	defaultValue := aValue
]

{ #category : 'accessing' }
AbstractSetting >> description [
	"Answer the value of description"

	^ description
		ifNil: [ '' ]
		ifNotNil: [ description translated ]
]

{ #category : 'accessing' }
AbstractSetting >> description: aString [

	description := aString
]

{ #category : 'old setting builder compatibility' }
AbstractSetting >> dialog: anObject [

	self flag: 'What does mean a dialog for a group?'.
]

{ #category : 'testing' }
AbstractSetting >> hasDefault [
	
	^ self defaultValue notNil
]

{ #category : 'testing' }
AbstractSetting >> hasDefaultValue [

	^ self realValue = self defaultValue
]

{ #category : 'testing' }
AbstractSetting >> hasParent [
	
	^ parent isNotNil
]

{ #category : 'testing' }
AbstractSetting >> hasTarget [
	
	^ target isNotNil
]

{ #category : 'old setting builder compatibility' }
AbstractSetting >> hasValue [

	^ true
]

{ #category : 'comparing' }
AbstractSetting >> hash [

	^ self species hash bitXor: self name hash
]

{ #category : 'old setting builder compatibility' }
AbstractSetting >> iconName: aString [

	self flag: 'ToDo'
]

{ #category : 'testing' }
AbstractSetting >> isGroup [
	
	^ false
]

{ #category : 'testing' }
AbstractSetting >> isTransient [
	"A transient setting is never stored"

	^ isTransient ifNil: [ false ]
]

{ #category : 'accessing' }
AbstractSetting >> isTransient: aBoolean [

	isTransient := aBoolean
]

{ #category : 'old setting builder compatibility' }
AbstractSetting >> item [

	self 
		deprecated: 'Use self instead. #Pharo14'
		transformWith: '`@receiver item' -> '`@receiver'.
		
	^ self
]

{ #category : 'accessing' }
AbstractSetting >> label [

	^ label
		  ifNil: [ self name translated ]
		  ifNotNil: [ label translated ]
]

{ #category : 'accessing' }
AbstractSetting >> label: aString [

	label := aString
]

{ #category : 'accessing' }
AbstractSetting >> name [

	^ name ifNil: [ 'Unnamed ' , self class name ]
]

{ #category : 'accessing' }
AbstractSetting >> name: aName [

	name := aName asSymbol
]

{ #category : 'old setting builder compatibility' }
AbstractSetting >> noOrdering [

	self flag: 'ToDo'
]

{ #category : 'accessing' }
AbstractSetting >> order [

	^ order
]

{ #category : 'accessing' }
AbstractSetting >> order: aNumber [

	order := aNumber
]

{ #category : 'accessing' }
AbstractSetting >> parent [
	
	^ parent
]

{ #category : 'accessing' }
AbstractSetting >> parent: aSymbol [
	
	self
		deprecated: 'Use #parentName: instead. #Pharo14'
		transformWith: '`@receiver parent: `@aSymbol' -> '`@receiver parentName: `@aSymbol'.

	self parentName: aSymbol
]

{ #category : 'accessing' }
AbstractSetting >> parentGroup: aSettingGroup [
	
	parent := aSettingGroup
]

{ #category : 'accessing' }
AbstractSetting >> parentName [
	
	^ parentName
]

{ #category : 'accessing' }
AbstractSetting >> parentName: aSymbol [
	
	parentName := aSymbol
]

{ #category : 'printing' }
AbstractSetting >> printOn: aStream [

	aStream
		<< '['
		<< self className
		<< ': ';
		<< self name asString;
		<< ']'
]

{ #category : 'accessing' }
AbstractSetting >> readBlock: aBlock [

	readBlock := aBlock 
]

{ #category : 'accessing' }
AbstractSetting >> realTarget [

	(self hasTarget not and: [ self hasParent not ]) 
		ifTrue: [ ^ self defaultTarget ].

	^ target isSymbol
		  ifTrue: [ Smalltalk globals at: target ifAbsent: [ ] ]
		  ifFalse: [ target ]
]

{ #category : 'accessing' }
AbstractSetting >> settingIdentifier [
	"identifier is a string because it can contains many #"
	
	^ String streamContents: [ :aStream |
			self settingIdentifierOn: aStream ]
]

{ #category : 'printing' }
AbstractSetting >> settingIdentifierOn: aWriteStream [ 

	parent
		ifNotNil: [ parent settingIdentifierOn: aWriteStream ]
		ifNil: [ parentName ifNotNil: [ aWriteStream print: parentName ] ].
	aWriteStream print: self name
]

{ #category : 'old setting builder compatibility' }
AbstractSetting >> settingNodeIdentifier [
	
	self 
		deprecated: 'Use #defaultValue: instead. #Pharo14'
		transformWith: '`@receiver settingNodeIdentifier' -> '`@receiver settingIdentifier'.
		
	^ self settingIdentifier
]

{ #category : 'old setting builder compatibility' }
AbstractSetting >> settingNodeIdentifierOn: aWriteStream [ 

	self 
		deprecated: 'Use #settingIdentifierOn: instead. #Pharo14'
		transformWith: '`@receiver settingNodeIdentifierOn: `@aWriteStream' -> '`@receiver settingIdentifierOn: `@aWriteStream'
]

{ #category : 'accessing' }
AbstractSetting >> settingReceiver [

	(self hasTarget not and: [ self hasParent ])
		ifTrue: [ ^ self parent settingReceiver ifNil: [ self defaultTarget ] ].

	^ self targetSelector
		  ifNil: [ self realTarget ]
		  ifNotNil: [ self realTarget perform: self targetSelector ]
]

{ #category : 'accessing' }
AbstractSetting >> target [

	^ target
]

{ #category : 'accessing' }
AbstractSetting >> target: anObject [

	target := anObject
]

{ #category : 'accessing' }
AbstractSetting >> targetSelector [

	^ targetSelector
]

{ #category : 'accessing' }
AbstractSetting >> targetSelector: aSelector [

	targetSelector := aSelector
]

{ #category : 'accessing' }
AbstractSetting >> writeBlock: aBlock [

	writeBlock := aBlock 
]
