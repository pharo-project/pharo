"
I represent abstract tree structure for concrete menu implementation like context menu, toolbar, halo menu and etc..
I am created for commands which have given #commandActivatorClass:
	menu := CmdMenu activatedBy: aCommandActivatorClass

Then I can build items for given context:
	menu buildInContext: aToolContext.
This method can be called multiple times for different contexts. It allows to build single menu for multiple parts of application. For example toolbar menu can include commands for all visible widgets.   
When menu is built you can inspect #rootGroup to analyze structure.

Concrete activator packages extend me to build concrete type of end user menu. For example context menu activator ask me to #buildContextMenu. Then I ask all my items to build context menu. And at the end commands receive context menu message where they build menu item morphs. 
 
Internal Representation and Key Implementation Points.

    Instance Variables
	commandActivatorClass:		<CmdMenuCommandActivator class>
	rootCommandClass:		<CmdCommand class>
	rootGroup:		<CmdMenuGroup class>
"
Class {
	#name : #CmdMenu,
	#superclass : #Object,
	#instVars : [
		'rootGroup',
		'activationStrategyClass'
	],
	#category : #'Commander-Core'
}

{ #category : #'instance creation' }
CmdMenu class >> activatedBy: aCommandActivationStrategyClass [

	^self new 
		activationStrategyClass: aCommandActivationStrategyClass
]

{ #category : #accessing }
CmdMenu >> activationStrategyClass [
	^ activationStrategyClass
]

{ #category : #accessing }
CmdMenu >> activationStrategyClass: anObject [
	activationStrategyClass := anObject
]

{ #category : #building }
CmdMenu >> buildCommandItemInContext: aToolContext activatingBy: anActivationStrategy [

	| commandActivator group |
	commandActivator := anActivationStrategy newActivatorFor: aToolContext.
	
	group := self ensureGroup: anActivationStrategy menuGroup.
	group addItem: (CmdCommandMenuItem activatingBy: commandActivator)
]

{ #category : #building }
CmdMenu >> buildInContext: aToolContext [

	activationStrategyClass 
		visibleInstancesInContext: aToolContext
		do: [ :activation | 
			self buildCommandItemInContext: aToolContext activatingBy: activation]
]

{ #category : #building }
CmdMenu >> ensureGroup: menuGroupClass [

	| parentGroup group |
	rootGroup class = menuGroupClass ifTrue: [ ^rootGroup ].
	rootGroup findGroupLike: menuGroupClass ifExists: [:existing | ^existing].
	
	parentGroup := self ensureGroup: menuGroupClass parentGroup.
	group := menuGroupClass new.
	parentGroup addItem: group.
	^group
]

{ #category : #accessing }
CmdMenu >> groupsWithCommandLike: aCommandClass [ 
	
	| result |
	result := rootGroup allGroupsWhich: [ :each | each includesCommand: aCommandClass ].
	(rootGroup includesCommand: aCommandClass)
		ifTrue: [ result add: rootGroup ].
	^result
]

{ #category : #initialization }
CmdMenu >> initialize [
	super initialize.
	
	rootGroup := CmdRootMenuGroup new
]

{ #category : #testing }
CmdMenu >> isActive [
	^rootGroup isActive
]

{ #category : #testing }
CmdMenu >> isEmpty [
	^rootGroup isEmpty
]

{ #category : #accessing }
CmdMenu >> itemsWithCommandLike: aCommandClass [ 
	| groups |
	groups := self groupsWithCommandLike: aCommandClass.
	^groups 
		flatCollect: [ :each | { each. each commandItemLike: aCommandClass } ]
]

{ #category : #accessing }
CmdMenu >> rootGroup [
	^ rootGroup
]

{ #category : #accessing }
CmdMenu >> rootGroup: anObject [
	rootGroup := anObject
]

{ #category : #accessing }
CmdMenu >> rootItems [
	^rootGroup contents
]
