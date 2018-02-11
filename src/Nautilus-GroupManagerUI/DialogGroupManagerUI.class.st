"
Dialog UI for the group manager
"
Class {
	#name : #DialogGroupManagerUI,
	#superclass : #DialogWindow,
	#instVars : [
		'groupManager',
		'textModel',
		'groupTreeModel',
		'groupTree'
	],
	#category : #'Nautilus-GroupManagerUI'
}

{ #category : #'instance creation' }
DialogGroupManagerUI class >> on: aModel [

	^ self basicNew 
		groupManager: aModel;
		initialize.
]

{ #category : #private }
DialogGroupManagerUI >> addAGroup [

	| group |
	group := self groupManager groups createAnEmptyStaticGroup.
	groupTree updateList.
	groupTreeModel hardlySelectItem: group.
	GroupAnnouncer uniqueInstance announce: (AGroupHasBeenAdded group: group into: self)
]

{ #category : #'buttons behavior' }
DialogGroupManagerUI >> addAction [

	self addAGroup
]

{ #category : #'buttons behavior' }
DialogGroupManagerUI >> addLabel [
	
	^ 'Create'
]

{ #category : #'button behavior' }
DialogGroupManagerUI >> addSelectedPackageInSelectedGroup [
	| selection |
	selection := self groupTreeModel selection selectedNodePath.
	selection ifNil: [ ^ self ].
	selection last isGroup 
		ifTrue: [ 
			self selectedGroup
				ifNotNil: [ :group | 
					self groupManager add: self selectedElements into: group.
					groupTree updateList.
					groupTreeModel hardlySelectItem: group ] ]
]

{ #category : #private }
DialogGroupManagerUI >> applyChanges [

	self valid
]

{ #category : #'item creation' }
DialogGroupManagerUI >> buildAddButton [
	^ IconicButton new
		target: self;
		actionSelector: #addAction;
		labelGraphic: (self iconNamed: #add);
		color: Color transparent;
		helpText: 'Add a new group';
		borderWidth: 0;
		yourself
]

{ #category : #'item creation' }
DialogGroupManagerUI >> buildAddSelectedPackageInSelectedGroupButton [
	^ IconicButton new
		target: self;
		actionSelector: #addSelectedPackageInSelectedGroup;
		labelGraphic: (self iconNamed: #down);
		color: Color transparent;
		helpText: 'Add the selected package into the selected group';
		borderWidth: 0;
		yourself
]

{ #category : #'item creation' }
DialogGroupManagerUI >> buildEmptyGroupButton [
	^ IconicButton new
		target: self;
		actionSelector: #emptySelectedGroup;
		labelGraphic: (self iconNamed: #delete);
		color: Color transparent;
		helpText: 'Empty the selected group';
		borderWidth: 0;
		yourself
]

{ #category : #tree }
DialogGroupManagerUI >> buildGroupsTree [

	groupTree := (MorphTreeMorph on: self groupTreeModel)
		beSingle;
		autoDeselection: true;
		rowInset: 4;
		columnInset: 4;
		getMenuSelector: #menu:shifted:;
		nodeStringGetter: #name;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
		
	^groupTree buildContents.
]

{ #category : #'item creation' }
DialogGroupManagerUI >> buildPackageTree [
	| morph |
	self groupManager announcer when: MorphTreeSelectionChanged send: #itemSelected: to: self.
	self groupManager ghostString: 'Search...'.
	self groupManager rootItems: RPackageOrganizer default packages sorted.
	morph := self groupManager defaultMorphIn: World.
	^ morph
]

{ #category : #'item creation' }
DialogGroupManagerUI >> buildRemoveButton [
	^ IconicButton new
		target: self;
		actionSelector: #removeAction;
		labelGraphic: (self iconNamed: #removeIcon);
		color: Color transparent;
		helpText: 'Remove the selected group';
		borderWidth: 0;
		yourself
]

{ #category : #'item creation' }
DialogGroupManagerUI >> buildRemoveSelectedPackageFromSelectedGroupButton [
	^ IconicButton new
		target: self;
		actionSelector: #removeSelectedPackageFromSelectedGroup;
		labelGraphic: (self iconNamed: #up);
		color: Color transparent;
		helpText: 'Remove the selected package from its group';
		borderWidth: 0;
		yourself
]

{ #category : #'item creation' }
DialogGroupManagerUI >> buildRenameButton [
	^ IconicButton new
		target: self;
		actionSelector: #renameAction;
		labelGraphic: (self iconNamed: #edit);
		color: Color transparent;
		helpText: 'Rename the selected group';
		borderWidth: 0;
		yourself
]

{ #category : #display }
DialogGroupManagerUI >> buildTextMorph [
	| textMorph |
	textMorph := self textModel newScrolledText
		beForPlainText;
		beReadOnly;
		hResizing: #spaceFill;
		yourself.
	self textModel setInitialText: self text.
	^ textMorph
]

{ #category : #display }
DialogGroupManagerUI >> centering [
	self left: ((World width / 2) - (self width /2)).
	self top: ((World height / 2) - (self height /2))
]

{ #category : #'button behavior' }
DialogGroupManagerUI >> emptySelectedGroup [
	| groupToRemove confirmation |
	self removeState
		ifFalse: [ ^ self ].
	groupToRemove := self groupTreeModel selection selectedNodePath last.
	groupToRemove isGroup
		ifTrue: [ 
			self selectedGroup
				ifNotNil: [ :group | 
					confirmation := UIManager default confirm: 'Do you really want to empty the group named ' , group name , ' ?'.
					confirmation
						ifFalse: [ ^ self ].
					group removeAllPackages.
					groupTree updateList.
					groupTreeModel hardlySelectItem: group ] ]
]

{ #category : #accessing }
DialogGroupManagerUI >> groupManager [

	^ groupManager
]

{ #category : #accessing }
DialogGroupManagerUI >> groupManager: aModel [

	groupManager := aModel
]

{ #category : #accessing }
DialogGroupManagerUI >> groupTree [
	^ groupTree
]

{ #category : #'item creation' }
DialogGroupManagerUI >> groupTreeBox [
	^ GroupboxMorph new
		addContentMorph: self buildGroupsTree;
		addContentMorph:
			(self
				newRow:
					{self buildAddButton.
					self buildRenameButton.
					self buildEmptyGroupButton.
					self buildRemoveButton});
		label: 'Choose a group:';
		hResizing: #spaceFill;
		vResizing: #spaceFill.
]

{ #category : #tree }
DialogGroupManagerUI >> groupTreeModel [
	^ groupTreeModel ifNil: [ groupTreeModel := self groupTreeModelClass new model: self ]
]

{ #category : #private }
DialogGroupManagerUI >> groupTreeModelClass [
	^ GroupCreatorTreeModel
]

{ #category : #tree }
DialogGroupManagerUI >> groups [
	^ self groupManager groups groups select: [ :group | group isFillable and: [ group isReadOnly not ] ]
]

{ #category : #display }
DialogGroupManagerUI >> initialExtent [
	^ 500 @ 600
]

{ #category : #display }
DialogGroupManagerUI >> isResizeable [
	^true
]

{ #category : #'event handling' }
DialogGroupManagerUI >> itemSelected: anAnnouncement [
	self groupManager selectedElements: anAnnouncement selection selectedItems
]

{ #category : #'item creation' }
DialogGroupManagerUI >> newContentMorph [
	^ self
		newColumn:
			{self packageTreeBox.
			self
				newRow:
					{self buildAddSelectedPackageInSelectedGroupButton.
					self newColumn: {self buildRemoveSelectedPackageFromSelectedGroupButton}}.
			(self newRow: {self groupTreeBox})
				hResizing: #spaceFill;
				vResizing: #spaceFill}
]

{ #category : #display }
DialogGroupManagerUI >> openInWorld [
	super openInWorld.
	self centering
]

{ #category : #'item creation' }
DialogGroupManagerUI >> packageTreeBox [
	^ GroupboxMorph new
		addContentMorph: self buildPackageTree;
		label: 'Select an item:';
		hResizing: #spaceFill;
		vResizing: #spaceFill
]

{ #category : #private }
DialogGroupManagerUI >> removeAGroup [
	self groupManager groups removeAGroup: self selectedGroup.
	groupTree updateList.
	groupTreeModel selection: nil.
	groupTreeModel changed: #selection
]

{ #category : #'buttons behavior' }
DialogGroupManagerUI >> removeAction [
	self removeState
		ifTrue: [ self removeAGroup ]
]

{ #category : #'button behavior' }
DialogGroupManagerUI >> removeSelectedPackageFromSelectedGroup [
	| packageToRemove selection |
	selection := self groupTreeModel selection selectedNodePath.
	selection ifNil: [ ^ self ].
	packageToRemove := selection last. 
	packageToRemove isGroup
		ifFalse: [ 
			self selectedGroup
				ifNotNil: [ :group | 
					self groupManager remove: {packageToRemove item} from: group.
					groupTree updateList.
					groupTreeModel hardlySelectItem: group ] ]
]

{ #category : #'buttons behavior' }
DialogGroupManagerUI >> removeState [

	^ self selectedGroup notNil
]

{ #category : #'buttons behavior' }
DialogGroupManagerUI >> renameAction [
self renameState ifTrue: [  
	self renameGroup ]
]

{ #category : #private }
DialogGroupManagerUI >> renameGroup [

	self groupManager groups renameAGroup: self selectedGroup.	
	groupTree updateList.
]

{ #category : #'buttons behavior' }
DialogGroupManagerUI >> renameState [

	^ self selectedGroup notNil
]

{ #category : #display }
DialogGroupManagerUI >> selectedElements [
	^ self groupManager selectedElements
]

{ #category : #private }
DialogGroupManagerUI >> selectedGroup [
	
	^ self selectedNode 
		ifNil: [ nil ]
		ifNotNil: [:node |
			node isGroup
				ifTrue: [ node item ]
				ifFalse: [ node parentNode item ]]
]

{ #category : #private }
DialogGroupManagerUI >> selectedNode [

	^ self groupTreeModel selectedNode
]

{ #category : #tree }
DialogGroupManagerUI >> setSelectedNodeItem: anItem [
	| node |
	node := groupTree listManager nodeMorphsWithAllNodeItems: {anItem}.
	groupTree listManager selectedItems: node.
	^ node
]

{ #category : #accessing }
DialogGroupManagerUI >> textModel [
	^ textModel ifNil: [ textModel := RubScrolledTextModel new interactionModel: self ]
]

{ #category : #private }
DialogGroupManagerUI >> updateSelectedNode [

	self changed: #removeState
]

{ #category : #private }
DialogGroupManagerUI >> valid [
]
