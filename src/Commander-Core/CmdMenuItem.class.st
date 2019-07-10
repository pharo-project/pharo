"
I represent abstract menu item.
I have two main subclasses: ClyComandMenuItem and CmdMenuGroup. Last subclass is a root of menu group hierarchy.

To support menu my subclasses implement following methods:
- name
- order. It defines position in menu. Larger value pushes item to the end of menu.
- isActive. It defines if item can be activated. 
- isEmpty. It defines if item has children. 
- isSimilarTo: anotherMenuItem. It defines if two items are similar.

Also different kinds of menu activation strategy extend me and my subclasses by methods to support concrete menu
"
Class {
	#name : #CmdMenuItem,
	#superclass : #Object,
	#instVars : [
		'parentGroup'
	],
	#category : #'Commander-Core'
}

{ #category : #accessing }
CmdMenuItem >> actualParentGroup [
	parentGroup isRoot ifTrue: [ ^parentGroup ].
	
	^parentGroup isInlined 
		ifTrue: [ parentGroup actualParentGroup ]
		ifFalse: [ parentGroup]
]

{ #category : #accessing }
CmdMenuItem >> description [
	self subclassResponsibility 
]

{ #category : #accessing }
CmdMenuItem >> findGroupLike: menuGroupClass ifExists: aBlockClosure [
	self subclassResponsibility 
]

{ #category : #accessing }
CmdMenuItem >> icon [
	self subclassResponsibility 
]

{ #category : #testing }
CmdMenuItem >> includesCommand: aCommandClass [

	self subclassResponsibility 
]

{ #category : #testing }
CmdMenuItem >> isActive [
	self subclassResponsibility 
]

{ #category : #testing }
CmdMenuItem >> isCommandItem [
	^self subclassResponsibility 
]

{ #category : #testing }
CmdMenuItem >> isEmpty [
	^false
]

{ #category : #testing }
CmdMenuItem >> isLastActiveItemInMenu [
	^parentGroup isLastActiveMenuItem: self
]

{ #category : #testing }
CmdMenuItem >> isLastInMenu [
	^parentGroup isLastMenuItem: self
]

{ #category : #testing }
CmdMenuItem >> isSimilarTo: aMenuItem [
	self subclassResponsibility 
]

{ #category : #accessing }
CmdMenuItem >> name [
	self subclassResponsibility 
]

{ #category : #accessing }
CmdMenuItem >> order [
	self subclassResponsibility 
]

{ #category : #accessing }
CmdMenuItem >> parentGroup [
	^ parentGroup
]

{ #category : #accessing }
CmdMenuItem >> parentGroup: anObject [
	parentGroup := anObject
]

{ #category : #accessing }
CmdMenuItem >> pathToRoot [
	^parentGroup 
		ifNil: [ ^#() ]
		ifNotNil: [ parentGroup pathToRoot, {parentGroup} ]
]

{ #category : #updating }
CmdMenuItem >> synchronizeContentWith: anotherMenuItem [
	self subclassResponsibility 
]
