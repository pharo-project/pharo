"
Browser to merge changes
"
Class {
	#name : 'MCMergeBrowser',
	#superclass : 'MCCodeTool',
	#instVars : [
		'conflicts',
		'merger',
		'selection'
	],
	#category : 'MonticelloGUI-Tools',
	#package : 'MonticelloGUI',
	#tag : 'Tools'
}

{ #category : 'instance creation' }
MCMergeBrowser class >> resolveConflictsInMerger: aMerger [
	| inst |
	inst := self new merger: aMerger.
	^ inst showModally ifNil: [false]
]

{ #category : 'morphic ui' }
MCMergeBrowser >> buttonSpecs [
	^ #((Merge merge 'Proceed with the merge' canMerge)
		 (Cancel cancel 'Cancel the merge')
		('All Newer' chooseAllNewerConflicts 'Choose all newer conflict versions')
		('All Older' chooseAllOlderConflicts 'Choose all older conflict versions')
		('Rest Local' chooseAllUnchosenLocal 'Choose local versions of all remaining conflicts')
		('Rest Remote' chooseAllUnchosenRemote 'Choose remote versions of all remaining conflicts')
)
]

{ #category : 'actions' }
MCMergeBrowser >> canMerge [
	^ merger isMerged
]

{ #category : 'actions' }
MCMergeBrowser >> cancel [
	self answer: false
]

{ #category : 'actions' }
MCMergeBrowser >> chooseAllNewerConflicts [
	"Notify the potential new state of canMerge."
	
	conflicts do: [ :ea | ea chooseNewer ].
	self changed: #text; changed: #list; changed: #canMerge
]

{ #category : 'actions' }
MCMergeBrowser >> chooseAllOlderConflicts [
	"Notify the potential new state of canMerge."
	
	conflicts do: [ :ea | ea chooseOlder ].
	self changed: #text; changed: #list; changed: #canMerge
]

{ #category : 'actions' }
MCMergeBrowser >> chooseAllUnchosenLocal [
	"Notify the potential new state of canMerge."
	
	conflicts do: [ :ea | ea isResolved ifFalse: [ ea chooseLocal ] ].
	self changed: #text; changed: #list; changed: #canMerge
]

{ #category : 'actions' }
MCMergeBrowser >> chooseAllUnchosenRemote [
	"Notify the potential new state of canMerge."
	
	conflicts do: [ :ea | ea isResolved ifFalse: [ ea chooseRemote ] ].
	self changed: #text; changed: #list; changed: #canMerge
]

{ #category : 'actions' }
MCMergeBrowser >> chooseLocal [
	"Notify the potential new state of canMerge."
	
	self conflictSelectionDo:
		[selection chooseLocal.
		self changed: #text; changed: #list; changed: #canMerge]
]

{ #category : 'actions' }
MCMergeBrowser >> chooseRemote [
	"Notify the potential new state of canMerge."
	
	self conflictSelectionDo:
		[selection chooseRemote.
		self changed: #text; changed: #list; changed: #canMerge]
]

{ #category : 'actions' }
MCMergeBrowser >> clearChoice [
	self conflictSelectionDo:
		[selection clearChoice.
		self changed: #text; changed: #list]
]

{ #category : 'actions' }
MCMergeBrowser >> conflictSelectionDo: aBlock [
	self selectionIsConflicted
		ifTrue: aBlock
		ifFalse: [self inform: 'You must have a conflict selected']
]

{ #category : 'morphic ui' }
MCMergeBrowser >> defaultLabel [
	^ 'Merge Browser'
]

{ #category : 'actions' }
MCMergeBrowser >> getConflictMenu: aMenu [
	selection remoteChosen
		ifTrue: [aMenu add: 'Undo keep change' target: self selector: #clearChoice]
		ifFalse: [aMenu add: 'Keep change' target: self selector: #chooseRemote].
	selection localChosen
		ifTrue: [aMenu add: 'Undo reject change' target: self selector: #clearChoice]	
		ifFalse: [aMenu add: 'Reject change' target: self selector: #chooseLocal].
	^ aMenu
]

{ #category : 'morphic ui' }
MCMergeBrowser >> getMenu: aMenu [
	selection ifNil: [^ aMenu].
	^ self selectionIsConflicted
		ifTrue: [self getConflictMenu: aMenu]
		ifFalse: [self getOperationMenu: aMenu]
]

{ #category : 'actions' }
MCMergeBrowser >> getOperationMenu: aMenu [
	^ aMenu
]

{ #category : 'actions' }
MCMergeBrowser >> innerButtonRow [
	^ self buttonRow:
		#((Keep chooseRemote 'keep the selected change' selectionIsConflicted)
		  (Reject chooseLocal 'reject the selected change' selectionIsConflicted))
]

{ #category : 'actions' }
MCMergeBrowser >> installSelection [

	| loader |
	selection ifNotNil: [
		loader := MCPackageLoader new.
		selection applyTo: loader.
		loader load ]
]

{ #category : 'selecting' }
MCMergeBrowser >> invert [
	items := items collect: [:ea | ea inverse].
	self changed: #list; changed: #text; changed: #selection
]

{ #category : 'accessing' }
MCMergeBrowser >> items [
	^ conflicts, items
]

{ #category : 'accessing' }
MCMergeBrowser >> list [
	^ self items collect: [:ea | ea summary]
]

{ #category : 'actions' }
MCMergeBrowser >> merge [
	merger isMerged
		ifFalse: [self inform: 'You must resolve all the conflicts first']
		ifTrue: [self answer: true] 
]

{ #category : 'actions' }
MCMergeBrowser >> merger: aMerger [
	merger := aMerger.
	items := aMerger operations asSortedCollection.
	conflicts := aMerger conflicts sort: [:a :b | a operation <= b operation].
]

{ #category : 'accessing' }
MCMergeBrowser >> patch: aPatch [
	items := aPatch operations asSortedCollection
]

{ #category : 'actions' }
MCMergeBrowser >> revertSelection [

	| loader |
	selection ifNotNil: [
		loader := MCPackageLoader new.
		selection inverse applyTo: loader.
		loader load ]
]

{ #category : 'subclassresponsibility' }
MCMergeBrowser >> selectedClass [
	| definition |
	selection ifNil: [ ^ nil ].
	(definition := selection definition) ifNil: [ ^ nil ].
	definition isMethodDefinition
		ifFalse: [ ^ nil ].
	^ Smalltalk globals at: definition className ifAbsent: [  ]
]

{ #category : 'subclassresponsibility' }
MCMergeBrowser >> selectedClassOrMetaClass [
	| definition |
	selection ifNil: [ ^nil ].
	(definition := selection definition) ifNil: [ ^nil ].
	definition isMethodDefinition ifFalse: [ ^nil ].
	^definition actualClass
]

{ #category : 'subclassresponsibility' }
MCMergeBrowser >> selectedMessageCategoryName [
	| definition |
	selection ifNil: [ ^nil ].
	(definition := selection definition) ifNil: [ ^nil ].
	definition isMethodDefinition ifFalse: [ ^nil ].
	^definition category
]

{ #category : 'subclassresponsibility' }
MCMergeBrowser >> selectedMessageName [
	| definition |
	selection ifNil: [ ^nil ].
	(definition := selection definition) ifNil: [ ^nil ].
	definition isMethodDefinition ifFalse: [ ^nil ].
	^definition  selector
]

{ #category : 'selecting' }
MCMergeBrowser >> selection [
	^ selection 
		ifNil: [0]
		ifNotNil: [self items indexOf: selection]
]

{ #category : 'morphic ui' }
MCMergeBrowser >> selection: aNumber [
	"Notify change of conflicts too."

	selection := aNumber = 0 ifFalse: [ self items at: aNumber ].
	self
		changed: #selection;
		changed: #text;
		changed: #selectionIsConflicted
]

{ #category : 'actions' }
MCMergeBrowser >> selectionIsConflicted [
	selection ifNil: [ ^ false ].
	^ selection isConflict
]

{ #category : 'text' }
MCMergeBrowser >> text [
	^ selection ifNil: [''] ifNotNil: [selection source]
]

{ #category : 'text' }
MCMergeBrowser >> text: aTextOrString [
	self changed: #text
]

{ #category : 'morphic ui' }
MCMergeBrowser >> widgetSpecs [
	"ToolBuilder doesn't know about innerButtonRow. Made explicit here."

	^#(
		((buttonRow) (0 0 1 0) (0 0 0 30))
		((listMorph:selection:menu: list selection methodListMenu:) (0 0 1 0.4) (0 30 0 0))
		((buttonRow: #((Keep chooseRemote 'keep the selected change' selectionIsConflicted)
		  (Reject chooseLocal 'reject the selected change' selectionIsConflicted))) (0 0.4 1 0.4) (0 0 0 32))
		((textMorph: text) (0 0.4 1 1) (0 32 0 0))
		)

]
