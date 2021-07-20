"
Used to build nodes from the settings framework
"
Class {
	#name : #SettingNodeBuilder,
	#superclass : #Object,
	#instVars : [
		'node',
		'builder'
	],
	#category : #'System-Settings-Browser'
}

{ #category : #'private - tree building' }
SettingNodeBuilder >> asParentWhile: aBlock [
	builder parent: self while: aBlock

]

{ #category : #accessing }
SettingNodeBuilder >> builder: aTreeBuilder [
	builder := aTreeBuilder
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> default: anObject [
	node item  default: anObject

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> description: aText [
	node item description: aText
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> dialog: aSymbol [
	node item dialog: aSymbol

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> domainValues: aCollection [ 
	node item domainValues: aCollection 

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> getSelector: aSymbol [
	node item getSelector: aSymbol

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> ghostHelp: aString [
	node item ghostHelp: aString

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> icon: aForm [
	node item icon: aForm
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> iconName: aSymbol [
	self icon: (self iconNamed: aSymbol)
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> label: aString [
	node item label: aString

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> name [
	^ node item name

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> name: aString [
	node item name: aString

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> noOrdering [
	node item noOrdering

]

{ #category : #accessing }
SettingNodeBuilder >> node [
	^ node
]

{ #category : #accessing }
SettingNodeBuilder >> node: aSettingTreeNode [
	node := aSettingTreeNode
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> notInStyle [
	node item notInStyle
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> order: aNumber [
	node item order: aNumber

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> parent: aSymbol [
	node parentName: aSymbol
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> parentName [
	^ node parentName
	
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> parentName: aSymbol [
	node parentName: aSymbol.
	
]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> precondition: aValuable [
	node item precondition: aValuable

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> range: anInterval [
	node item range: anInterval

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> script: aSymbol [
	node item script: aSymbol

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> selector: aSymbol [
	node item selector: aSymbol

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> setSelector: aSymbol [
	node item setSelector: aSymbol

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> target: anObject [
	node item target: anObject

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> targetSelector: aSelector [
	node item targetSelector: aSelector

]

{ #category : #'declaration accessing' }
SettingNodeBuilder >> type: anObject [
	node item type: anObject

]

{ #category : #'tree building' }
SettingNodeBuilder >> with: aBlock [
	self asParentWhile: aBlock
]
