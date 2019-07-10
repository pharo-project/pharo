"
I am a hierarchy of commands which opens context menu using specified tool context which was using to activate me.

Subclasses should be created to open special kind of menus represented by different kind of CmdMenuCommandActivationStrategy.
They should implement method #activationStrategy by returning a class for required type of menu.

Internal Representation and Key Implementation Points.

    Instance Variables
	context:		<CmdToolContext>
"
Class {
	#name : #CmdOpenContextMenuCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'context'
	],
	#category : #'Commander-Activators-ContextMenu'
}

{ #category : #testing }
CmdOpenContextMenuCommand class >> isAbstract [
	^self = CmdOpenContextMenuCommand 
]

{ #category : #execution }
CmdOpenContextMenuCommand >> activationStrategy [
	"Subclasses should return specific kind of activation strategy
	which will be used to collect context menu commands"
	self subclassResponsibility 
]

{ #category : #accessing }
CmdOpenContextMenuCommand >> context [
	^ context
]

{ #category : #accessing }
CmdOpenContextMenuCommand >> context: anObject [
	context := anObject
]

{ #category : #'context menu' }
CmdOpenContextMenuCommand >> createContextMenu [ 

	^self activationStrategy buildContextMenuFor: context tool inContext: context
]

{ #category : #accessing }
CmdOpenContextMenuCommand >> defaultMenuItemName [
	^'Open context menu'
]

{ #category : #execution }
CmdOpenContextMenuCommand >> execute [
	
	| menu |
	menu := self createContextMenu.
	menu hasItems ifFalse: [^self].
	
	menu setInvokingView: context tool.
	menu invokeModal
]

{ #category : #'context menu' }
CmdOpenContextMenuCommand >> fillContextMenu: aMenu using: aCommandActivator [
	"Here we need to take care because this command itself is added to menu of same context.
	So we will always enter into this method during submenu generation. 
	We catch this recursion using RecursionStopper. 
	It is easier than adding extra logic to menu building process"	
	| submenu |
	RecursionStopper during: [ 
		submenu := self createContextMenu.
		submenu hasItems ifFalse: [ ^self ].
		
		super fillContextMenu: aMenu using: aCommandActivator.
		aMenu lastItem subMenu: submenu
	]
]

{ #category : #execution }
CmdOpenContextMenuCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	
	context := aToolContext
]

{ #category : #'context menu' }
CmdOpenContextMenuCommand >> registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder [
	"Here we need to take care because this command itself is added to menu of same context.
	So we will always enter into this method during submenu generation. 
	We catch this recursion using RecursionStopper. 
	It is easier than adding extra logic to menu registration process"	
	| submenu |
	RecursionStopper during: [ 		
		submenu := self activationStrategy buildMenuInContext: context.		 
		submenu isActive ifTrue: [ 					
			super registerContextMenuItemsFor: aCommandItem withBuilder: aBuilder.
			"Registration builder uses #name to bind children to parent"
			submenu rootGroup name: aCommandItem name.
			submenu registerContextMenuItemsWithBuilder: aBuilder]		
	]
]
