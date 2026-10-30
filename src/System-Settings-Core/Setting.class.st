"
A Setting is an handler for a setting value.
It holds descriptive data about the setting.
"
Class {
	#name : 'Setting',
	#superclass : 'AbstractSetting',
	#instVars : [
		'getSelector',
		'setSelector',
		'precondition',
		'type'
	],
	#category : 'System-Settings-Core-Base-New',
	#package : 'System-Settings-Core',
	#tag : 'Base-New'
}

{ #category : 'accessing' }
Setting class >> validTypes [

	^ #( #string #boolean #integer #float #file #Password #Directory #FilePathEncoder #Label)
]

{ #category : 'old setting builder - with support on setting' }
Setting >> asParentWhile: aBlock [

	self builder parent: self while: aBlock
]

{ #category : 'private' }
Setting >> basicRealValue [

	^ readBlock 
		ifNil: [ self settingReceiver perform: self getSelector ]
		ifNotNil: [ readBlock value ]
]

{ #category : 'old setting builder compatibility' }
Setting >> default: aValue [

	self 
		deprecated: 'Use #defaultValue: instead. #Pharo14'
		transformWith: '`@receiver default: `@aValue' -> '`@receiver defaultValue: `@aValue'.
	self defaultValue: aValue
]

{ #category : 'accessing' }
Setting >> getSelector [

	^ getSelector ifNil: [ self name ]
]

{ #category : 'accessing' }
Setting >> getSelector: aSymbol [

	getSelector := aSymbol
]

{ #category : 'old setting builder compatibility' }
Setting >> ghostHelp: aString [

	self flag: 'ToDo'
]

{ #category : 'testing' }
Setting >> ifPreconditionTrue: trueBlock ifFalse: falseBlock [

	precondition ifNil: [ ^ trueBlock value ].
	^ precondition value
		ifTrue: [ trueBlock value ]
		ifFalse: [ falseBlock value ]
]

{ #category : 'accessing' }
Setting >> precondition: aValuable [

	precondition := aValuable
]

{ #category : 'accessing' }
Setting >> realValue [

	^ self basicRealValue ifNil: [
			  self setToDefault.
			  self basicRealValue ]
]

{ #category : 'accessing' }
Setting >> realValue: anObject [

	^ writeBlock 
		ifNil: [ self settingReceiver perform: self setSelector with: anObject ]
		ifNotNil: [ writeBlock value: anObject ]
]

{ #category : 'accessing' }
Setting >> selector: aSymbol [

	self getSelector: aSymbol.
	self setSelector: aSymbol asMutator
]

{ #category : 'accessing' }
Setting >> setSelector [

	^ setSelector ifNil: [
		  self getSelector ifNotNil: [ :selector | (selector , ':') asSymbol ] ]
]

{ #category : 'accessing' }
Setting >> setSelector: aSymbol [

	setSelector := aSymbol
]

{ #category : 'setting' }
Setting >> setToDefault [

	self realValue: self defaultValue
]

{ #category : 'accessing' }
Setting >> settingReceiver [

	(self hasTarget not and: [ self hasParent ])
		ifTrue: [ ^ self parent settingReceiver ifNil: [ self defaultTarget ] ].

	^ self targetSelector
		  ifNil: [ self realTarget ]
		  ifNotNil: [ self realTarget perform: self targetSelector ]
]

{ #category : 'accessing' }
Setting >> type [

	^ type
]

{ #category : 'accessing' }
Setting >> type: aSymbol [

	(self class validTypes includes: aSymbol)
		ifTrue: [ type := aSymbol ]
		ifFalse: [ UnknownSettingType signalType: aSymbol ]
]

{ #category : 'old setting builder - with support on setting' }
Setting >> with: aBlock [

	self asParentWhile: aBlock
]
