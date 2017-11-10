"
A PragmaMenuAndShortcutRegistration is the list items holder
"
Class {
	#name : #PragmaMenuAndShortcutRegistration,
	#superclass : #Object,
	#instVars : [
		'items',
		'currentRoot',
		'platform',
		'model'
	],
	#category : #MenuRegistration-Core
}

{ #category : #'instance creation' }
PragmaMenuAndShortcutRegistration class >> model: aModel [

	^ self new
		model: aModel;
		yourself
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> action: aBlock [

	self currentItem action: aBlock
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> arguments: anArray [

	self currentItem arguments: anArray
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> category: aByteSymbol [
	
	self currentItem
		category: aByteSymbol
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> category: aByteSymbol default: aKMModifiedShortcut do: aBlockClosure [ 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure 
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> category: aByteSymbol default: aKMModifiedShortcut do: aBlockClosure description: aByteString [ 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure;
		help: aByteString
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> category: aByteSymbol shortcut: aKMModifiedShortcut do: aBlockClosure [ 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure 
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> category: aByteSymbol shortcut: aKMModifiedShortcut do: aBlockClosure description: aByteString [ 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure;
		help: aByteString
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> createNewItem [
	
	items add: self itemClass new.
	
	self currentItem
		platform: platform;
		parent: currentRoot
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> currentItem [

	^ items last
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistration >> currentRoot [

	^ currentRoot
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistration >> currentRoot: aMenuRegistration [ 

	currentRoot := aMenuRegistration
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> default: aKMModifiedShortcut [
	
	self currentItem
		default: aKMModifiedShortcut
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> description: aByteString [ 
	
	self currentItem
		help: aByteString
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> do: aBlockClosure [
	
	self currentItem
		action: aBlockClosure
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistration >> enabled: aBoolean [
 
	self currentItem enabled: aBoolean
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> enabledBlock: aBlock [

	self currentItem enabledBlock: aBlock
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> group: aSymbol [ 
	
	self createNewItem.
	self currentItem group: aSymbol
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistration >> handOutItems [
	"we don't want to hold a reference to the items after they are hand out to the registration"

	| itemsCopy |
	itemsCopy := items copy.
	items removeAll.
	^ itemsCopy
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> help: aString [ 
	
	self currentItem help: aString 
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> icon: anIcon [
	
	self currentItem icon: anIcon
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> iconName: aSymbol [
	"instead of forcing clients to refer to an icon builder such Smalltalk ui icons 
	this message encapsulates it inside the builder itself. When removing uses of Smalltalk ui icons it avoid to force to subclass class with menu to inherit from Model."
	
	self icon: (Smalltalk ui icons iconNamed: aSymbol)
]

{ #category : #initialization }
PragmaMenuAndShortcutRegistration >> initialize [

	super initialize.

	items := OrderedCollection new.
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> item: aSymbol [ 
	
	self createNewItem.
	self currentItem item: aSymbol
]

{ #category : #private }
PragmaMenuAndShortcutRegistration >> itemClass [

	^ PragmaMenuAndShortcutRegistrationItem
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistration >> items [

	^ items
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> keyText: aString [
	self currentItem keyText: aString
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> keyText: aString if: aBoolean [
	aBoolean ifFalse: [ ^ self ].
	self keyText: aString
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> label: aString [
	
	self currentItem label: aString
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistration >> model [

	^ model
]

{ #category : #accessing }
PragmaMenuAndShortcutRegistration >> model: aModel [

	model := aModel.
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> order: anInteger [ 
	
	self currentItem order: anInteger 
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> parent: aSymbol [ 
	
	self currentItem parent: aSymbol 
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> platform: anArray [
 
	anArray isEmpty
		ifTrue: [ platform := #all ]
		ifFalse: [ platform := anArray first ]
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> precondition: aBlock [ 
	
	self currentItem precondition: aBlock 
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> selector: aSelector [ 
	
	self currentItem selector: aSelector
]

{ #category : #'keymapping protocol' }
PragmaMenuAndShortcutRegistration >> shortcut: aSymbol [
	
	self createNewItem.
	self currentItem shortcut: aSymbol
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> target: aSymbol [ 

	self currentItem target: aSymbol 
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> with: aBlock [
	| old itemOrGroup |
	old := self currentRoot.
	itemOrGroup := self currentItem item ifNil: [ self currentItem group ].
	self currentRoot: itemOrGroup.
	[ aBlock value ]
		ensure: [ self currentRoot: old ]
]

{ #category : #'menu protocol' }
PragmaMenuAndShortcutRegistration >> withSeparatorAfter [

	self currentItem withSeparatorAfter
]
