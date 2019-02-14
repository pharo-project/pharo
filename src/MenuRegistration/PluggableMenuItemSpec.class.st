"
I'm a spec for an item menu. The World menu pragma based registration uses it for now. I'm moved in this package so that later we can decide what to do with it. 
We could probably avoid MenuSpec and just use MenuMorph instead. 
"
Class {
	#name : #PluggableMenuItemSpec,
	#superclass : #Object,
	#instVars : [
		'label',
		'action',
		'checked',
		'enabled',
		'separator',
		'subMenu',
		'icon',
		'enabledBlock',
		'keyText',
		'help',
		'name'
	],
	#category : #'MenuRegistration-Core'
}

{ #category : #accessing }
PluggableMenuItemSpec >> action [
	"Answer the action associated with the receiver"
	^action
]

{ #category : #accessing }
PluggableMenuItemSpec >> action: aMessageSend [
	"Answer the action associated with the receiver"
	action := aMessageSend
]

{ #category : #accessing }
PluggableMenuItemSpec >> checked [
	"Answer whether the receiver is checked"
	^checked 
]

{ #category : #accessing }
PluggableMenuItemSpec >> checked: aBool [
	"Indicate whether the receiver is checked"
	checked := aBool.
]

{ #category : #accessing }
PluggableMenuItemSpec >> enabled [
	"Answer whether the receiver is enabled"
	^ enabledBlock
		ifNil:[enabled ifNil: [ true ]]
		ifNotNil: [ enabledBlock value ]
]

{ #category : #accessing }
PluggableMenuItemSpec >> enabled: aBool [
	"Indicate whether the receiver is enabled"
	enabled := aBool
]

{ #category : #accessing }
PluggableMenuItemSpec >> enabledBlock [

	^ enabledBlock
]

{ #category : #accessing }
PluggableMenuItemSpec >> enabledBlock: aBlock [

	enabledBlock := aBlock
]

{ #category : #accessing }
PluggableMenuItemSpec >> hasCheckBox [	
	^ checked notNil
]

{ #category : #accessing }
PluggableMenuItemSpec >> help [
	"Answer the message to get the help texts of this element."
	^ help
]

{ #category : #accessing }
PluggableMenuItemSpec >> help: aSymbol [ 
	"Indicate the message to retrieve the help texts of this element."
	help := aSymbol
]

{ #category : #accessing }
PluggableMenuItemSpec >> icon [
	^ icon
]

{ #category : #accessing }
PluggableMenuItemSpec >> icon: aForm [
	 icon := aForm
]

{ #category : #accessing }
PluggableMenuItemSpec >> keyText [
	^keyText
]

{ #category : #accessing }
PluggableMenuItemSpec >> keyText: aString [
	keyText := aString.
]

{ #category : #accessing }
PluggableMenuItemSpec >> label [
	"Answer the receiver's label"
	^label
]

{ #category : #accessing }
PluggableMenuItemSpec >> label: aString [
	"Set the receiver's label"
	label := aString
]

{ #category : #accessing }
PluggableMenuItemSpec >> name [
	^ name
]

{ #category : #accessing }
PluggableMenuItemSpec >> name: anObject [
	name := anObject
]

{ #category : #accessing }
PluggableMenuItemSpec >> separator [
	"Answer whether the receiver should be followed by a separator"
	^separator ifNil:[false]
]

{ #category : #accessing }
PluggableMenuItemSpec >> separator: aBool [
	"Indicate whether the receiver should be followed by a separator"
	separator := aBool.
]

{ #category : #accessing }
PluggableMenuItemSpec >> subMenu [
	"Answer the receiver's subMenu"
	^subMenu
]

{ #category : #accessing }
PluggableMenuItemSpec >> subMenu: aMenuSpec [
	"Answer the receiver's subMenu"
	subMenu := aMenuSpec
]
