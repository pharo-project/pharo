"
I am a root of command activation hierarchy which supposed to represent commands in kind of menu.
I provide information about name, group and position of command inside menu:
- menuItemName. If it is not specified I ask command for #defaultMenuItemName.
- menuItemOrder
- menuGroup
My subclasses provide meaning of concrete menu type. It could be context menu, toolbar menu, halo menu and etc..
To build any kind of end user menu my subclasses first build abstract CmdMenu. It represents tree structure of concrete menu implemetation.
Concrete menu activations extend abstract menu to delegate item building to underlying commands and groups.

Menu groups are represented by subclasses of CmdMenuGroup. They are used as classes to declare activation strategy. Instances are only created during menu building.
Groups are containers of command items and other groups.
Each group defines #parentGroup on class side. By default it is CmdRootMenuGroup. Subclasses can override it to define deep tree structure.

I provide suitable methods to declare activators:
	ConcreteMenuCommandActivation byRootGroupItemFor: YourAppContext 
	ConcreteMenuCommandActivation byRootGroupItemOrder: aNumber for: YourAppContext
	ConcreteMenuCommandActivation byItemOf: menuGroupClass for: YourAppContext
	ConcreteMenuCommandActivation byItemOf: menuGroupClass order: aNumber for: YourAppContext

Larger order pushes command to the end of menu. Groups are also define order by instance side method #order.

Internal Representation and Key Implementation Points.

    Instance Variables
	menuGroup:		<CmdMenuGroup class>
	menuItemName:		<String>
	menuItemOrder:		<Number>
"
Class {
	#name : #CmdMenuCommandActivationStrategy,
	#superclass : #CmdCommandActivationStrategy,
	#instVars : [
		'menuItemOrder',
		'menuGroup',
		'menuItemName'
	],
	#category : #'Commander-Core'
}

{ #category : #'menu creation' }
CmdMenuCommandActivationStrategy class >> buildMenuInContext: aToolContext [
	| menu |
	menu := CmdMenu activatedBy: self.
	menu buildInContext: aToolContext.
	^ menu
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byItem: itemName of: menuGroupClass for: aToolContextClass [
	^(self byItemOf: menuGroupClass for: aToolContextClass)
		 menuItemName: itemName
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byItem: itemName of: menuGroupClass order: aNumber for: aToolContextClass [
	^(self byItemOf: menuGroupClass order: aNumber for: aToolContextClass)
		menuItemName: itemName
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byItemOf: menuGroupClass for: aToolContextClass [
	^(self for: aToolContextClass)
		 menuGroup: menuGroupClass
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byItemOf: menuGroupClass order: aNumber for: aToolContextClass [
	^(self byItemOf: menuGroupClass for: aToolContextClass)
		menuItemOrder: aNumber
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byRootGroupItem: itemName for: aToolContextClass [
	^(self byRootGroupItemFor: aToolContextClass)
		menuItemName: itemName
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byRootGroupItem: itemName order: aNumber for: aToolContextClass [
	^(self byRootGroupItemOrder: aNumber for: aToolContextClass)
		menuItemName: itemName
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byRootGroupItemFor: aToolContextClass [
	^self byItemOf: CmdRootMenuGroup for: aToolContextClass
]

{ #category : #'instance creation' }
CmdMenuCommandActivationStrategy class >> byRootGroupItemOrder: aNumber for: aToolContextClass [
	^self byItemOf: CmdRootMenuGroup order: aNumber for: aToolContextClass
]

{ #category : #initialization }
CmdMenuCommandActivationStrategy >> initialize [
	super initialize.
	menuItemOrder := 1000.
	menuGroup := CmdRootMenuGroup
]

{ #category : #accessing }
CmdMenuCommandActivationStrategy >> menuGroup [
	^ menuGroup
]

{ #category : #accessing }
CmdMenuCommandActivationStrategy >> menuGroup: anObject [
	menuGroup := anObject
]

{ #category : #accessing }
CmdMenuCommandActivationStrategy >> menuItemName [
	^ menuItemName
]

{ #category : #accessing }
CmdMenuCommandActivationStrategy >> menuItemName: anObject [
	menuItemName := anObject
]

{ #category : #accessing }
CmdMenuCommandActivationStrategy >> menuItemNameFor: aCommand [
	^ menuItemName ifNil: [ aCommand defaultMenuItemName ]
]

{ #category : #accessing }
CmdMenuCommandActivationStrategy >> menuItemOrder [
	^ menuItemOrder
]

{ #category : #accessing }
CmdMenuCommandActivationStrategy >> menuItemOrder: anObject [
	menuItemOrder := anObject
]

{ #category : #printing }
CmdMenuCommandActivationStrategy >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	menuGroup printOn: aStream.
	aStream nextPutAll: '; '.
	menuItemOrder printOn: aStream.
	aStream nextPut: $).
]
