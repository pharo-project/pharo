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
	#name : 'PragmaSetting',
	#superclass : 'Model',
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
		'dialog'
	],
	#category : 'System-Settings-Core-Base',
	#package : 'System-Settings-Core',
	#tag : 'Base'
}

{ #category : 'comparing' }
PragmaSetting >> = other [
	^ self species = other species
		and: [self name = other name]
]

{ #category : 'visitor' }
PragmaSetting >> acceptSettings: aVisitor [
	^ aVisitor visitPragmaSetting: self
]

{ #category : 'user interface' }
PragmaSetting >> asString [
	^ self name
]

{ #category : 'user interface' }
PragmaSetting >> defaultIcon [
	^ self iconNamed: #smallConfiguration
]

{ #category : 'accessing' }
PragmaSetting >> description [
	"Answer the value of description"

	^ description
		ifNil: [description := '']
		ifNotNil: [ description ]
]

{ #category : 'accessing' }
PragmaSetting >> description: aString [
	"Set the value of description"

	description := aString
]

{ #category : 'user interface' }
PragmaSetting >> dialog [
	^ dialog
]

{ #category : 'user interface' }
PragmaSetting >> dialog: aValuable [
	dialog := aValuable
]

{ #category : 'user interface' }
PragmaSetting >> enabled [
	^ true
]

{ #category : 'export' }
PragmaSetting >> exportSettingAction [

	^ nil
]

{ #category : 'accessing' }
PragmaSetting >> hasDefault [
	^ false
]

{ #category : 'accessing' }
PragmaSetting >> hasValue [
	^ false
]

{ #category : 'comparing' }
PragmaSetting >> hash [
	^ self species hash bitXor: self name hash
]

{ #category : 'user interface' }
PragmaSetting >> icon [
	^ icon ifNil: [icon := self defaultIcon]
]

{ #category : 'user interface' }
PragmaSetting >> icon: aForm [
	icon := aForm
]

{ #category : 'accessing' }
PragmaSetting >> label [
	^ label ifNil: [self name] ifNotNil: [label]
]

{ #category : 'accessing' }
PragmaSetting >> label: aString [
	label := aString
]

{ #category : 'accessing' }
PragmaSetting >> name [
	^ name 
		ifNil: [ 'Unnamed ' , self class name ] 
		ifNotNil: [ name ]
]

{ #category : 'accessing' }
PragmaSetting >> name: aName [
	name := aName
]

{ #category : 'user interface' }
PragmaSetting >> noOrdering [
	ordering := false
]

{ #category : 'accessing' }
PragmaSetting >> order [
	^ order
]

{ #category : 'accessing' }
PragmaSetting >> order: aNumber [
	order := aNumber
]

{ #category : 'user interface' }
PragmaSetting >> ordering [
	^ ordering ifNil: [ordering := true]
]

{ #category : 'accessing' }
PragmaSetting >> precondition [
	^ precondition ifNil: [precondition := [true]]
]

{ #category : 'accessing' }
PragmaSetting >> precondition: aValuable [
	precondition := aValuable
]

{ #category : 'printing' }
PragmaSetting >> printOn: aStream [

	aStream
		<< '[Setting: ';
		<< self name asString;
		<< ']'
]

{ #category : 'accessing' }
PragmaSetting >> realTarget [
	^ target isSymbol
		ifTrue: [ Smalltalk globals at: target ifAbsent: [  ] ]
		ifFalse: [ target ]
]

{ #category : 'accessing' }
PragmaSetting >> settingReceiver [
	^ self targetSelector ifNil: [self realTarget] ifNotNil: [self realTarget perform: self targetSelector]
]

{ #category : 'accessing' }
PragmaSetting >> target [
	^ target
]

{ #category : 'accessing' }
PragmaSetting >> target: anObject [

	target := anObject
]

{ #category : 'accessing' }
PragmaSetting >> targetSelector [
	^ targetSelector
]

{ #category : 'accessing' }
PragmaSetting >> targetSelector: aSelector [
	targetSelector := aSelector
]
