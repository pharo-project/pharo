"
A PragmaSetting is a holder for a setting. A setting is declared in a class method which contains a pragma (as examples, <menu> or <setting>). 
Settings can be collected on the fly by a PragmaCollector and browsed by a SettingBrowser. Settings are organized in trees.

Instance Variables
	allowedInStyle:		<Object>
	description:		<Object>
	dialog:		<Object>
	icon:		<Object>
	label:		<Object>
	name:		<Object>
	order:		<Object>
	ordering:		<Object>
	precondition:		<Object>
	target:		<Object>
	targetSelector:		<Object>

allowedInStyle
	- xxxxx

description
	- xxxxx

dialog
	- xxxxx

icon
	- xxxxx

label
	- xxxxx

name
	- xxxxx

order
	- xxxxx

ordering
	- xxxxx

precondition
	- xxxxx

target
	- xxxxx

targetSelector
	- xxxxx

"
Class {
	#name : #PragmaSetting,
	#superclass : #Model,
	#instVars : [
		'precondition',
		'target',
		'targetSelector',
		'name',
		'label',
		'description',
		'order',
		'icon',
		'ordering',
		'dialog',
		'allowedInStyle'
	],
	#category : #'System-Settings-Core-Base'
}

{ #category : #comparing }
PragmaSetting >> = other [ 
	^ self species = other species
		and: [self name = other name]
]

{ #category : #visitor }
PragmaSetting >> acceptSettings: aVisitor [
	^ aVisitor visitPragmaSetting: self
]

{ #category : #accessing }
PragmaSetting >> allowedInStyle [
	^ allowedInStyle ifNil: [allowedInStyle := self hasValue]
]

{ #category : #'user interface' }
PragmaSetting >> asString [
	^ self name
]

{ #category : #'user interface' }
PragmaSetting >> chooseFileDirectory [
	| result |
	result := self theme chooseDirectoryIn: self currentWorld title: 'Choose a directory' path: nil.
	result ifNotNil: [ self realValue: result  ].
]

{ #category : #'user interface' }
PragmaSetting >> chooseFilePath [
    | result |
    result := self theme chooseFullFileNameIn: self currentWorld title: 'Choose a file' extensions: nil path: nil preview: true.
    result ifNotNil: [ self realValue: result.]
]

{ #category : #'user interface' }
PragmaSetting >> defaultIcon [
	^ self iconNamed: #smallConfigurationIcon
]

{ #category : #accessing }
PragmaSetting >> description [
	"Answer the value of description"

	^ description 
		ifNil: [description := ''] 
		ifNotNil: [ description translated ]

]

{ #category : #accessing }
PragmaSetting >> description: aString [
	"Set the value of description"

	description := aString 
]

{ #category : #'user interface' }
PragmaSetting >> dialog [
	^ dialog
]

{ #category : #'user interface' }
PragmaSetting >> dialog: aValuable [
	dialog := aValuable
]

{ #category : #'user interface' }
PragmaSetting >> enabled [
	^ true
]

{ #category : #export }
PragmaSetting >> exportSettingAction [
	
	^ nil
]

{ #category : #'user interface' }
PragmaSetting >> fontButtonLabel [
	| emphases |
	emphases := IdentityDictionary new at: nil put: 'Regular' translated;
				 at: 0 put: 'Regular' translated;
				 at: 1 put: 'Bold' translated;
				 at: 2 put: 'Italic' translated;
				 at: 4 put: 'Underlined' translated;
				 at: 8 put: 'Narrow' translated; 
				 at: 16 put: 'StruckOut' translated;
				 yourself.
	^ StringMorph contents: self realValue familyName , ' '
			, (emphases
					at: self realValue emphasis
					ifAbsent: ['']) , ' ' , self realValue pointSize asString font: self realValue
]

{ #category : #'user interface' }
PragmaSetting >> getFont [
	^ FontChooser
		openWithWindowTitle: 'Font for ' , self name
		for: self
		setSelector: #realValue:
		getSelector: #realValue 
]

{ #category : #accessing }
PragmaSetting >> hasDefault [
	^ false
]

{ #category : #accessing }
PragmaSetting >> hasEditableList [
	^ false
]

{ #category : #accessing }
PragmaSetting >> hasValue [
	^ false
]

{ #category : #comparing }
PragmaSetting >> hash [
	^ self species hash bitXor: self name hash 
]

{ #category : #'user interface' }
PragmaSetting >> icon [
	^ icon ifNil: [icon := self defaultIcon]
]

{ #category : #'user interface' }
PragmaSetting >> icon: aForm [ 
	icon := aForm
]

{ #category : #'user interface' }
PragmaSetting >> inputMorphFor: aContainer [ 
	^ self inputWidget
		ifNotNil: [:iw | 
			( self theme newRowIn: self currentWorld for: {iw})
				 clipSubmorphs: true;
				 hResizing: #shrinkWrap;
				 cellInset: 0;
				 yourself]
]

{ #category : #'user interface' }
PragmaSetting >> inputWidget [
	"return the default widget for the input a the setting"
	^ self dialog
		ifNotNil: [:d | d numArgs = 0
				ifTrue: [d value]
				ifFalse: [d value: self]] 
]

{ #category : #export }
PragmaSetting >> isExportable [

	^ false
]

{ #category : #accessing }
PragmaSetting >> label [
	^ label ifNil: [self name translated] ifNotNil: [label translated]
]

{ #category : #accessing }
PragmaSetting >> label: aString [
	label := aString
]

{ #category : #'user interface' }
PragmaSetting >> labelMorphFor: aContainer [
	^ StringMorph contents: (aContainer model viewedLabelOfSetting: self)
]

{ #category : #accessing }
PragmaSetting >> name [
	^ name ifNil: [super name] ifNotNil: [name]
]

{ #category : #accessing }
PragmaSetting >> name: aName [
	name := aName
]

{ #category : #'user interface' }
PragmaSetting >> noOrdering [
	ordering := false
]

{ #category : #'user interface' }
PragmaSetting >> notInStyle [
	allowedInStyle := false
]

{ #category : #accessing }
PragmaSetting >> order [
	^ order
]

{ #category : #accessing }
PragmaSetting >> order: aNumber [
	order := aNumber
]

{ #category : #'user interface' }
PragmaSetting >> ordering [
	^ ordering ifNil: [ordering := true]
]

{ #category : #accessing }
PragmaSetting >> precondition [
	^ precondition ifNil: [precondition := [true]].
]

{ #category : #accessing }
PragmaSetting >> precondition: aValuable [
	precondition := aValuable
]

{ #category : #accessing }
PragmaSetting >> realTarget [
	^ target isSymbol
		ifTrue: [ Smalltalk globals at: target ifAbsent: [  ] ]
		ifFalse: [ target ]
]

{ #category : #accessing }
PragmaSetting >> settingReceiver [
	^ self targetSelector ifNil: [self realTarget] ifNotNil: [self realTarget perform: self targetSelector]
]

{ #category : #accessing }
PragmaSetting >> target [
	^ target
]

{ #category : #accessing }
PragmaSetting >> target: anObject [
	target := anObject
]

{ #category : #accessing }
PragmaSetting >> targetSelector [
	^ targetSelector
]

{ #category : #accessing }
PragmaSetting >> targetSelector: aSelector [
	targetSelector := aSelector
]
