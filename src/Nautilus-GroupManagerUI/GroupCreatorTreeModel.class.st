"
Tree model for group creation in Nautilus
"
Class {
	#name : #GroupCreatorTreeModel,
	#superclass : #MorphTreeModel,
	#instVars : [
		'model'
	],
	#category : #Nautilus-GroupManagerUI
}

{ #category : #'drag and drop' }
GroupCreatorTreeModel >> acceptDroppingMorph: transferMorph event: evt inMorph: listMorph [
	"transferMorph passenger complexContents item pour récupérer le package"

	| group package target |
	(listMorph scrollerSubMorphFromPoint: evt position) complexContents
		ifNil: [ 
			self model groupManager patternModel acceptEditsInView.
			^ self ]
		ifNotNil: [ :node | target := node ].
	target isGroup
		ifTrue: [ group := target item ]
		ifFalse: [ group := target parentNode item ].
	package := transferMorph passenger complexContents item.
	self model groupManager add: {package} into: group.
	self model groupTree updateList.
	self model groupManager patternModel acceptEditsInView
]

{ #category : #'drag and drop' }
GroupCreatorTreeModel >> dropEnabled [
	^ true
]

{ #category : #private }
GroupCreatorTreeModel >> groups [

	^ self model groups
]

{ #category : #accessing }
GroupCreatorTreeModel >> groupsManager [

	^ self model groupsManager
]

{ #category : #private }
GroupCreatorTreeModel >> hardlySelectItem: anItem [

	self selectNodePath: ((self setSelectedNodeItem: anItem)collect:#complexContents)
]

{ #category : #accessing }
GroupCreatorTreeModel >> model [

	^ model
]

{ #category : #accessing }
GroupCreatorTreeModel >> model: anObject [

	model := anObject
]

{ #category : #'t - accessing' }
GroupCreatorTreeModel >> rootItems [
	
	^ self groups
]

{ #category : #'t - accessing' }
GroupCreatorTreeModel >> rootNodeClassFromItem: anItem [

	^ GroupNode
]

{ #category : #'t - accessing' }
GroupCreatorTreeModel >> selection [

	| list |
	list := self rootItems.
	(list notEmpty and: [ super selection isNil ])
		ifTrue: [	self hardlySelectItem: list first ].
	^ super selection
]

{ #category : #'t - accessing' }
GroupCreatorTreeModel >> selection: aNode [
	super selection: aNode.
	self model updateSelectedNode
]

{ #category : #'t - accessing' }
GroupCreatorTreeModel >> setSelectedNodeItem: anItem [

	^ self model setSelectedNodeItem: anItem
]

{ #category : #'drag and drop' }
GroupCreatorTreeModel >> wantsDroppedMorph: transferMorph event: evt inMorph: listMorph [
	(transferMorph isKindOf: TransferMorph)
		ifFalse: [ ^ false ].
	(self model groupManager dragTransferTypeForMorph: listMorph) = #SelectedPackage
		ifFalse: [ ^ false ].
	^ true
]
