"
I am the root of the Calypso command activation strategies which represent a command as a menu item. I provide the name, the group and the order of the menu item. My subclasses define the kind of menu they belong to, such as a toolbar or a browser tab.
"
Class {
	#name : 'ClyMenuCommandActivationStrategy',
	#superclass : 'ClyCommandActivationStrategy',
	#instVars : [
		'menuItemOrder',
		'menuGroup',
		'menuItemName'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'building' }
ClyMenuCommandActivationStrategy class >> buildContextMenuFor: aMorph inContext: aToolContext [
	"The context menu of this kind of activation, built as the Commander2 tree and drawn by its renderer."
	^ ClyCommandGroupMenuRenderer new
		renderTree: (ClyCommandGroupBuilder new buildMenuFor: self context: aToolContext pureRoots: #())
		on: aMorph
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byItem: itemName of: menuGroupClass for: aToolContextClass [
	^(self byItemOf: menuGroupClass for: aToolContextClass)
		 menuItemName: itemName
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byItem: itemName of: menuGroupClass order: aNumber for: aToolContextClass [
	^(self byItemOf: menuGroupClass order: aNumber for: aToolContextClass)
		menuItemName: itemName
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byItemOf: menuGroupClass for: aToolContextClass [
	^(self for: aToolContextClass)
		 menuGroup: menuGroupClass
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byItemOf: menuGroupClass order: aNumber for: aToolContextClass [
	^(self byItemOf: menuGroupClass for: aToolContextClass)
		menuItemOrder: aNumber
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byRootGroupItem: itemName for: aToolContextClass [
	^(self byRootGroupItemFor: aToolContextClass)
		menuItemName: itemName
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byRootGroupItem: itemName order: aNumber for: aToolContextClass [
	^(self byRootGroupItemOrder: aNumber for: aToolContextClass)
		menuItemName: itemName
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byRootGroupItemFor: aToolContextClass [
	^self byItemOf: ClyRootMenuGroup for: aToolContextClass
]

{ #category : 'instance creation' }
ClyMenuCommandActivationStrategy class >> byRootGroupItemOrder: aNumber for: aToolContextClass [
	^self byItemOf: ClyRootMenuGroup order: aNumber for: aToolContextClass
]

{ #category : 'testing' }
ClyMenuCommandActivationStrategy class >> isAbstract [

	^ self == ClyMenuCommandActivationStrategy
]

{ #category : 'building' }
ClyMenuCommandActivationStrategy class >> registerContextMenuOn: aBuilder inContext: aToolContext [
	"Registers the context menu of this kind of activation in a pragma menu builder, from the Commander2 tree."
	ClyCommandGroupMenuRenderer new
		registerTree: (ClyCommandGroupBuilder new buildMenuFor: self context: aToolContext pureRoots: #())
		on: aBuilder rootName: nil
]

{ #category : 'initialization' }
ClyMenuCommandActivationStrategy >> initialize [
	super initialize.
	menuItemOrder := 1000.
	menuGroup := ClyRootMenuGroup
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuGroup [
	^ menuGroup
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuGroup: anObject [
	menuGroup := anObject
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuItemName [
	^ menuItemName
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuItemName: anObject [
	menuItemName := anObject
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuItemNameFor: aCommand [
	^ menuItemName ifNil: [ aCommand name ]
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuItemOrder [
	^ menuItemOrder
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuItemOrder: anObject [
	menuItemOrder := anObject
]

{ #category : 'accessing' }
ClyMenuCommandActivationStrategy >> menuItemOrderFor: aCommand [
	"The order of the menu item of the command. ClyCommandEntry can ask it to the command."

	^ menuItemOrder
]

{ #category : 'printing' }
ClyMenuCommandActivationStrategy >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	menuGroup printOn: aStream.
	aStream nextPutAll: '; '.
	menuItemOrder printOn: aStream.
	aStream nextPut: $)
]
