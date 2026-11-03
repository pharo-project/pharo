"
I represent a Setting Category. I hold some data on the category.
I'm identified by my name. Settinng refer to me by this name.
"
Class {
	#name : 'SettingsGroup',
	#superclass : 'AbstractSetting',
	#instVars : [
		'isActive',
		'isActivable'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'private - building' }
SettingsGroup >> asParentWhile: aBlock [

	self builder parent: self while: aBlock
]

{ #category : 'testing' }
SettingsGroup >> hasDefaultValue [

	"Group should manage its visibily. When done, this methods should check the value of the visibility against the default value"
	^ true
]

{ #category : 'initialization' }
SettingsGroup >> initialize [ 

	super initialize.
	isActivable := false.
]

{ #category : 'initialization' }
SettingsGroup >> initializeBlocks [

	readBlock ifNil: [ readBlock := [ self settingReceiver perform: self name ] ].
	writeBlock ifNil: [ writeBlock := [ :value | self settingReceiver perform: self name , ':' withArguments: { value } ] ]
]

{ #category : 'accessing' }
SettingsGroup >> isActivable [

	^ isActivable
]

{ #category : 'accessing' }
SettingsGroup >> isActivable: aBoolean [

	isActivable := aBoolean.
	isActivable ifTrue: [ self initializeBlocks ]
]

{ #category : 'accessing' }
SettingsGroup >> isActive [

	^ isActive
]

{ #category : 'accessing' }
SettingsGroup >> isActive: aBoolean [

	isActive := aBoolean.
	self isActivable: true
]

{ #category : 'testing' }
SettingsGroup >> isGroup [
	
	^ true
]

{ #category : 'accessing' }
SettingsGroup >> realValue [
	"Tells if the group is active or not.
	realValue will be used only if readBlock is not nil, in this case we will return block's value"

	^ readBlock ifNotNil: [ readBlock value ]
]

{ #category : 'accessing' }
SettingsGroup >> realValue: aBoolean [

	"if aBoolean is true, that means that the group is active"
	
   writeBlock value: aBoolean.
]

{ #category : 'accessing' }
SettingsGroup >> settingReceiver [

	^ self targetSelector
		  ifNil: [ self target ifNil: [ self defaultTarget ]]
		  ifNotNil: [ self target perform: self targetSelector ]
]

{ #category : 'building' }
SettingsGroup >> with: aBlock [

	self asParentWhile: aBlock
]
