"
A SettingTree is responsible for the building of system setting trees from a collection of pragma.
Built tree nodes are instances of SettingNode. Built trees are suitable for tree browsing with the help of a MorphTreeMorph (see SettingBrowser>>treeMorphIn: and SettingBrowser>>roots to know how a SettingBrowser is initializing its MorphTreeMorph with the help of a SettingTree). 

Below, two examples of tree building with a SettingCollector are shown.
---------------
(SettingTree acceptableKeywords: #(#'systemsettings')) settingTreeRoots. ""get system setting trees""
---------------

Instance Variables
	collector		<PragmaCollector>
	nodeList		<A list of SettingNode>

"
Class {
	#name : #SettingTree,
	#superclass : #Model,
	#instVars : [
		'collector',
		'nodeList',
		'persistence'
	],
	#category : #'System-Settings-Browser'
}

{ #category : #'instance creation' }
SettingTree class >> acceptableKeywords: aCollectionOfStrings [
	^ self new acceptableKeywords: aCollectionOfStrings 

]

{ #category : #accessing }
SettingTree >> acceptableKeywords: keywords [
	self collector filter: [:prg | prg methodClass isMeta and: [keywords includes: prg selector]].
	nodeList := nil.
	self collector reset.

]

{ #category : #checking }
SettingTree >> checkForUnknownParent [
	self unclassified
		ifNotEmpty: [ :nodesWithUnknownParent | 
			| builder g |
			builder := SettingTreeBuilder new.
			g := (builder group: #nodesWithUnknownParent) node.
			self nodeList addFirst: g.
			g model: self.
			g item label: '*** Unclassified ***'.
			g item icon: (self iconNamed: #smallDebugIcon).
			g item order: 0.0.
			nodesWithUnknownParent
				do: [ :node | node parentName: #nodesWithUnknownParent ] ]
]

{ #category : #checking }
SettingTree >> checkForUnknownTarget [
	"check only root because other nodes with unknown target inherits it from its parent"
	self settingTreeRoots do: [:node | node item target ifNil: [node item target: (node pragma ifNotNil: [:prg | prg methodClass instanceSide])]].
	self settingTreeRoots do: [:node | node checkForUnknownTarget].

]

{ #category : #querying }
SettingTree >> childrenOf: aNode [
	| children idx order |
	"Get aNode children. Assign children order if one child has an order which is not nil"
	children := self nodeList select: [ :p | p parentName = aNode settingDeclaration name ].
	children
		detect: [ :n | n order notNil ]
		ifFound: [ :firstWithOrder | 
			idx := children indexOf: firstWithOrder.
			order := firstWithOrder order.
			idx > 1
				ifTrue: [ 
					idx - 1 to: 1 do: [ :pos | 
						(children at: pos) order: order - 1.
						order := order - 1 ] ].
			order := firstWithOrder order.
			idx + 1 to: children size do: [ :pos | 
				(children at: pos) order ifNil: [ (children at: pos) order: order + 1 ] ifNotNil: [ order := (children at: pos) order ].
				order := order + 1 ] ].
	^ children
]

{ #category : #accessing }
SettingTree >> collector [
	^ collector ifNil: [collector := PragmaCollector new]
]

{ #category : #querying }
SettingTree >> deeplyDetect: aBlock [ 
	self settingTreeRoots
		do: [:sub | (sub deeplyDetect: aBlock)
				ifNotNil: [:found | ^ found]].
	^ nil 
]

{ #category : #querying }
SettingTree >> deeplySelect: aBlock [
	^ self deeplySelect: aBlock in: OrderedCollection new

]

{ #category : #querying }
SettingTree >> deeplySelect: aBlock in: aCollection [ 
	self settingTreeRoots
		do: [:aRoot | aRoot deeplySelect: aBlock in: aCollection]. 
	^ aCollection
]

{ #category : #'private tree building' }
SettingTree >> emptyNodeNamed: aSymbol [
	(self nodeNamed: aSymbol)

]

{ #category : #accessing }
SettingTree >> itemSortBlock [
	^ [:a :b | 
		((a order notNil and: [b order notNil]) and: [a order ~= b order])
			ifTrue: [a order < b order]
			ifFalse: [((a order isNil and: [b order isNil]) or: [a order = b order])
				ifTrue: [a label < b label]
				ifFalse: [a order ifNil: [false] ifNotNil: [true]]]]
]

{ #category : #accessing }
SettingTree >> nodeList [
	| builder |
	^ nodeList 
		ifNil: [
			builder := SettingTreeBuilder new.
			self pragmasDo: [:p | builder buildPragma: p].
			nodeList := builder nodeList.
			nodeList do: [:n | n model: self].
			self checkForUnknownParent.
			self checkForUnknownTarget.
			self nodeList]
]

{ #category : #'private tree building' }
SettingTree >> nodeNamed: aSymbol [
	^ self nodeNamed: aSymbol ifAbsent: []

]

{ #category : #'private tree building' }
SettingTree >> nodeNamed: aSymbol ifAbsent: aBlock [
	^ self nodeList detect: [:d | d settingDeclaration name = aSymbol] ifNone: aBlock
]

{ #category : #accessing }
SettingTree >> persistence [
	^ persistence ifNil: [ persistence := SystemSettingsPersistence settingTree: self ]
]

{ #category : #querying }
SettingTree >> pragmasDo: aBlock [
	^  self collector do: aBlock
]

{ #category : #querying }
SettingTree >> retainedNodesFromList: aListOfNodes [
	| retained |
	retained := OrderedCollection new.
	aListOfNodes do: [:n | [n item precondition value ifTrue: [retained add: n]] on: Error do: [retained add: n]].
	^ retained
]

{ #category : #querying }
SettingTree >> settingTreeRoots [
	^ self retainedNodesFromList: ((self nodeList select: [:n | n parentName isNil]) asArray sort: self sortBlock) asOrderedCollection.

]

{ #category : #accessing }
SettingTree >> sortBlock [
	^ [:a :b | self itemSortBlock value: a settingDeclaration value: b settingDeclaration]
]

{ #category : #persistence }
SettingTree >> storeSettingNodes [
	self persistence storeSettingNodes
]

{ #category : #querying }
SettingTree >> unclassified [
	^ self nodeList select: [:node | node parentName notNil and: [node parentNode isNil]].
]

{ #category : #accessing }
SettingTree >> updateList [
	nodeList := nil.

]

{ #category : #persistence }
SettingTree >> updateSettingNodes [
	self persistence updateSettingNodes
]

{ #category : #updating }
SettingTree >> whenChangedSend: aSelector to: aTarget [
	self collector whenChangedSend: aSelector to: aTarget.
	self collector whenResetSend: aSelector to: aTarget.
]
