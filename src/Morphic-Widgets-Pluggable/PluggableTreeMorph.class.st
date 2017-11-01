"
A pluggable tree morph.
"
Class {
	#name : #PluggableTreeMorph,
	#superclass : #SimpleHierarchicalListMorph,
	#instVars : [
		'roots',
		'selectedWrapper',
		'getRootsSelector',
		'getChildrenSelector',
		'hasChildrenSelector',
		'getLabelSelector',
		'getIconSelector',
		'getSelectedPathSelector',
		'getHelpSelector',
		'dropItemSelector',
		'wantsDropSelector',
		'dragItemSelector'
	],
	#category : 'Morphic-Widgets-Pluggable'
}

{ #category : #morphic }
PluggableTreeMorph >> acceptDroppingMorph: aMorph event: evt [
	| item dropTarget |

	dropItemSelector 
		ifNil:[
			model 
				acceptDroppingMorph: aMorph 
				event: evt 
				inMorph: self ]
		ifNotNil: [
			item := aMorph passenger.
			dropTarget := (self itemFromPoint: evt position) withoutListWrapper.
			model perform: dropItemSelector with: item with: dropTarget ].
		
	evt hand releaseMouseFocus: self.
	Cursor normal show.

]

{ #category : #'node access' }
PluggableTreeMorph >> balloonTextForNode: node [
	getHelpSelector ifNil:[^nil].
	^model perform: getHelpSelector with: node item
]

{ #category : #'node access' }
PluggableTreeMorph >> contentsOfNode: node [
	| children |
	getChildrenSelector ifNil:[^#()].
	children := model perform: getChildrenSelector with: node item.
	^children collect:[:item| PluggableTreeItemNode with: item model: self]
]

{ #category : #accessing }
PluggableTreeMorph >> dragItemSelector [
	^dragItemSelector
]

{ #category : #accessing }
PluggableTreeMorph >> dragItemSelector: aSymbol [
	dragItemSelector := aSymbol.
	aSymbol ifNotNil:[self dragEnabled: true].
]

{ #category : #accessing }
PluggableTreeMorph >> dropItemSelector [
	^dropItemSelector
]

{ #category : #accessing }
PluggableTreeMorph >> dropItemSelector: aSymbol [
	dropItemSelector := aSymbol.
	aSymbol ifNotNil:[self dropEnabled: true].
]

{ #category : #'node access' }
PluggableTreeMorph >> dropNode: srcNode on: dstNode [
	dropItemSelector ifNil:[^nil].
	model perform: dropItemSelector with: srcNode item with: dstNode item
]

{ #category : #accessing }
PluggableTreeMorph >> getChildrenSelector [
	^getChildrenSelector
]

{ #category : #accessing }
PluggableTreeMorph >> getChildrenSelector: aSymbol [
	getChildrenSelector := aSymbol.
]

{ #category : #accessing }
PluggableTreeMorph >> getHelpSelector [
	^getHelpSelector
]

{ #category : #accessing }
PluggableTreeMorph >> getHelpSelector: aSymbol [
	getHelpSelector := aSymbol
]

{ #category : #accessing }
PluggableTreeMorph >> getIconSelector [
	^getIconSelector
]

{ #category : #accessing }
PluggableTreeMorph >> getIconSelector: aSymbol [
	getIconSelector := aSymbol
]

{ #category : #accessing }
PluggableTreeMorph >> getLabelSelector [
	^getLabelSelector
]

{ #category : #accessing }
PluggableTreeMorph >> getLabelSelector: aSymbol [
	getLabelSelector := aSymbol
]

{ #category : #accessing }
PluggableTreeMorph >> getRootsSelector [
	^getRootsSelector
]

{ #category : #accessing }
PluggableTreeMorph >> getRootsSelector: aSelector [
	getRootsSelector := aSelector.
	self update: getRootsSelector.
]

{ #category : #accessing }
PluggableTreeMorph >> getSelectedPathSelector [
	^getSelectedPathSelector
]

{ #category : #accessing }
PluggableTreeMorph >> getSelectedPathSelector: aSymbol [
	getSelectedPathSelector := aSymbol.
]

{ #category : #accessing }
PluggableTreeMorph >> hasChildrenSelector [
	^hasChildrenSelector
]

{ #category : #accessing }
PluggableTreeMorph >> hasChildrenSelector: aSymbol [
	hasChildrenSelector := aSymbol
]

{ #category : #'node access' }
PluggableTreeMorph >> hasNodeContents: node [
	hasChildrenSelector ifNil: [ ^ node contents isNotEmpty ].
	^ model perform: hasChildrenSelector with: node item
]

{ #category : #'node access' }
PluggableTreeMorph >> iconOfNode: node [
	getIconSelector ifNil:[^nil].
	^model perform: getIconSelector with: node item
]

{ #category : #'node access' }
PluggableTreeMorph >> isDraggableNode: node [
	^true
]

{ #category : #accessing }
PluggableTreeMorph >> keystrokeActionSelector [
	^keystrokeActionSelector
]

{ #category : #accessing }
PluggableTreeMorph >> keystrokeActionSelector: aSymbol [
	keystrokeActionSelector := aSymbol
]

{ #category : #'node access' }
PluggableTreeMorph >> printNode: node [
	getLabelSelector ifNil:[^node item printString].
	^model perform: getLabelSelector with: node item
]

{ #category : #accessing }
PluggableTreeMorph >> roots [
	^roots
]

{ #category : #accessing }
PluggableTreeMorph >> roots: anArray [
	roots := anArray collect:[:item| PluggableTreeItemNode with: item model: self].
	self list: roots.
]

{ #category : #updating }
PluggableTreeMorph >> selectPath: path in: listItem [
	path isEmpty ifTrue: [^self setSelectedMorph: nil].
	listItem withSiblingsDo: [:each | 
		(each complexContents item = path first) ifTrue: [
			each isExpanded ifFalse: [
				each toggleExpandedState.
				self adjustSubmorphPositions.
			].
			each changed.
			path size = 1 ifTrue: [
				^self setSelectedMorph: each
			].
			each firstChild ifNil: [^self setSelectedMorph: nil].
			^self selectPath: path allButFirst in: each firstChild
		].
	].
	^self setSelectedMorph: nil


]

{ #category : #selection }
PluggableTreeMorph >> setSelectedMorph: aMorph [
	selectedWrapper := aMorph complexContents.
	self selection: selectedWrapper.
	setSelectionSelector ifNotNil:[
		model 
			perform: setSelectionSelector 
			with: (selectedWrapper ifNotNil:[selectedWrapper item]).
	].
]

{ #category : #accessing }
PluggableTreeMorph >> setSelectedSelector [
	^setSelectionSelector
]

{ #category : #accessing }
PluggableTreeMorph >> setSelectedSelector: aSymbol [
	setSelectionSelector := aSymbol
]

{ #category : #morphic }
PluggableTreeMorph >> startDrag: anEvent [ 
	| aTransferMorph itemMorph passenger |
	
	self dragEnabled
		ifTrue: [itemMorph := scroller submorphs
						detect: [:any | any highlightedForMouseDown]
						ifNone: []].
	(itemMorph isNil
			or: [anEvent hand hasSubmorphs])
		ifTrue: [^ self].
	itemMorph highlightForMouseDown: false.
	itemMorph ~= self selectedMorph
		ifTrue: [self setSelectedMorph: itemMorph].
		
	passenger := dragItemSelector
		ifNil: [ self model
			dragPassengerFor: itemMorph withoutListWrapper 
			inMorph: self ]
		ifNotNil: [
			self model 
				perform: dragItemSelector 
				withEnoughArguments: { itemMorph withoutListWrapper. self } ].
			
	passenger
		ifNotNil: [aTransferMorph := self model transferFor: passenger from: self.
			aTransferMorph dragTransferType: #dragTransfer.
			aTransferMorph align: aTransferMorph draggedMorph center with: anEvent position.
			anEvent hand grabMorph: aTransferMorph].
	anEvent hand releaseMouseFocus: self
]

{ #category : #updating }
PluggableTreeMorph >> update: what [
	what ifNil:[^self].
	what == getRootsSelector ifTrue:[
		self roots: (model perform: getRootsSelector)
	].
	what == getSelectedPathSelector ifTrue:[
		^self selectPath: (model perform: getSelectedPathSelector)
			in: (scroller submorphs at: 1 ifAbsent: [^self]) 
	].
	^super update: what
]

{ #category : #accessing }
PluggableTreeMorph >> wantsDropSelector [
	^wantsDropSelector
]

{ #category : #accessing }
PluggableTreeMorph >> wantsDropSelector: aSymbol [
	wantsDropSelector := aSymbol
]

{ #category : #morphic }
PluggableTreeMorph >> wantsDroppedMorph: aMorph event: anEvent [
	
	aMorph dragTransferType == #dragTransfer ifFalse: [ ^ false ].
	
	^ wantsDropSelector 
		ifNil: [ model
			wantsDroppedMorph: aMorph 
			event: anEvent 
			inMorph: self ]
		ifNotNil: [
			model 
				perform: wantsDropSelector 
				with: aMorph passenger ]
]

{ #category : #'node access' }
PluggableTreeMorph >> wantsDroppedNode: srcNode on: dstNode [
	dropItemSelector ifNil:[^false].
	wantsDropSelector ifNil:[^true].
	^(model perform: wantsDropSelector with: srcNode with: dstNode) == true
]
