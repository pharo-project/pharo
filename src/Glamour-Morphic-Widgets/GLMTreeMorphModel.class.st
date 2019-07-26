"
ClassTree new openOn: Collection

"
Class {
	#name : #GLMTreeMorphModel,
	#superclass : #PaginatedMorphTreeModel,
	#instVars : [
		'roots',
		'glamourPresentation',
		'childrenBlock',
		'displayFormat',
		'tagsToFilterBy',
		'glmAnnouncer',
		'inputText',
		'amountToFilterBy',
		'defaultPageSize',
		'columnToSortBy',
		'stopStepping'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #'drag and drop' }
GLMTreeMorphModel >> acceptDroppingMorph: transferMorph event: evt inMorph: listMorph [ 
	| targetItem |
	targetItem := (listMorph scrollerSubMorphFromPoint: evt position) complexContents item.	
	^ self glamourPresentation accept: transferMorph passenger droppedOnItem: targetItem
]

{ #category : #accessing }
GLMTreeMorphModel >> allKeystrokeActions [
	^ (self glamourPresentation allActions, self glamourPresentation allSelectionActions) select: [ :action | 
		action hasShortcut ]
]

{ #category : #accessing }
GLMTreeMorphModel >> allMenuActions [
	^ (self glamourPresentation allSelectionActions select: [:action | action hasTitle ]) 
]

{ #category : #accessing }
GLMTreeMorphModel >> amountToFilterBy [
	^ nil "amountToFilterBy ifNil: [self glamourPresentation amountToShow]"
]

{ #category : #accessing }
GLMTreeMorphModel >> amountToFilterBy: aNumber [ 
	amountToFilterBy := aNumber
]

{ #category : #announcements }
GLMTreeMorphModel >> announce: anAnnouncement [
	self announcer announce: anAnnouncement 
]

{ #category : #accessing }
GLMTreeMorphModel >> announcer [
	^glmAnnouncer ifNil: [ glmAnnouncer := GLMAnnouncer new ]
]

{ #category : #accessing }
GLMTreeMorphModel >> childrenBlock [
	^ childrenBlock ifNil: [ childrenBlock := self glamourPresentation children isNil 
		ifFalse: [self glamourPresentation children] ifTrue: [OrderedCollection new] ]
]

{ #category : #accessing }
GLMTreeMorphModel >> childrenBlock: aBlock [
	childrenBlock := aBlock 
]

{ #category : #accessing }
GLMTreeMorphModel >> defaultPageSize [
	^ defaultPageSize
]

{ #category : #accessing }
GLMTreeMorphModel >> defaultPageSize: anInteger [
	defaultPageSize := anInteger
]

{ #category : #accessing }
GLMTreeMorphModel >> displayFormat [
	^ displayFormat ifNil: [displayFormat := self glamourPresentation format]
]

{ #category : #accessing }
GLMTreeMorphModel >> displayFormat: aBlock [ 
	displayFormat := aBlock 
]

{ #category : #accessing }
GLMTreeMorphModel >> displayValue [
	
	^ columnToSortBy 
		ifNil: [self glamourPresentation displayValue]
		ifNotNil: [self glamourPresentation displayValue sorted: self sortBlock]
]

{ #category : #'search and filtering' }
GLMTreeMorphModel >> doSearchOrFilter [
	inputText isEmptyOrNil
		ifTrue: [ self resetInput ]
		ifFalse:[ 
			self glamourPresentation allowsFilter 
				ifTrue: [ self executeFilter ]
				ifFalse: [ self executeSearch ] ].

]

{ #category : #'drag and drop' }
GLMTreeMorphModel >> dragEnabled [
	^ self glamourPresentation allowsItemDrag
]

{ #category : #'drag and drop' }
GLMTreeMorphModel >> dragPassengerFor: item inMorph: listMorph [
	^ self glamourPresentation transformedDraggedItem: item complexContents item 
]

{ #category : #'drag and drop' }
GLMTreeMorphModel >> dragTransferType [
	^ #Glamour
]

{ #category : #'drag and drop' }
GLMTreeMorphModel >> dragTransferTypeForMorph: listMorph [
	^ self dragTransferType
]

{ #category : #'drag and drop' }
GLMTreeMorphModel >> dropEnabled [
	^ self glamourPresentation isDropTarget
]

{ #category : #'search and filtering' }
GLMTreeMorphModel >> executeFilter [
	self updateRoots
]

{ #category : #'search and filtering' }
GLMTreeMorphModel >> executeSearch [
	| newSelection check |
	newSelection := OrderedCollection new.
	roots do: [ :each | 
		check := self glamourPresentation searchStrategy value: self inputText value: each item.
		check ifTrue: [newSelection add: each path]].
	newSelection isEmpty 
		ifFalse: [ self selection: (MorphTreeMorphMultipleSelection new selectedNodePathList: newSelection) ]
		ifTrue: [self selection: nil]
]

{ #category : #callbacks }
GLMTreeMorphModel >> explicitSelection: anObject [ 
	"anObject is a domain object, not a Glamour related object"
	self glamourPresentation isMultiple 
		ifTrue: [ self explicitlySelectMultipleItems: anObject ]
		ifFalse: [ self explicitlySelectItem: anObject ]
]

{ #category : #'private selecting' }
GLMTreeMorphModel >> explicitlySelectItem: anObject [

	( self selectedItem notNil and: [ self selectedItem = anObject ] )
		ifTrue: [ ^ self ].
	( self selectedItem isNil and: [ anObject isNil ] )
		ifTrue: [ ^ self ].
	anObject
		ifNil: [ "we make sure to update the morph list by deselecting everything"
			self selectNodePath: nil.
			self selection: nil
			].
	self roots
		do: [ :eachRoot | 
			eachRoot
				withContentsDo: [ :each | 
					each item = anObject
						ifTrue: [ each expandParentPath.
							self selectNodePath: each path.
							^ self
							]
					]
			]
]

{ #category : #'private selecting' }
GLMTreeMorphModel >> explicitlySelectMultipleItems: aCollection [

	| newSelection collection |

	aCollection ifNil: [ ^ self ].
	collection := aCollection isCollection
		ifTrue: [ aCollection ]
		ifFalse: [ aCollection asOrderedCollection ].
	( self selection notNil
		and: [ self selection selectedItemOrItemsOrNil = collection asOrderedCollection ] )
		ifTrue: [ ^ self ].
	newSelection := OrderedCollection new.
	self roots
		do: [ :eachRoot | 
			eachRoot
				withContentsDo: [ :each | 
					( collection includes: each item )
						ifTrue: [ newSelection add: each path ]
					]
			].
	self selectAllNodePaths: newSelection
]

{ #category : #accessing }
GLMTreeMorphModel >> filteredRoots [
	| filtered |
	roots ifNil: [ self roots: self displayValue ].
	filtered := roots select: [ :each | each shouldBeDisplayed ].
	^ filtered
]

{ #category : #stepping }
GLMTreeMorphModel >> forceStopStepping [
	stopStepping := true.
	self changed.
]

{ #category : #accessing }
GLMTreeMorphModel >> glamourPresentation [
	^ glamourPresentation
]

{ #category : #accessing }
GLMTreeMorphModel >> glamourPresentation: anObject [
	glamourPresentation := anObject
]

{ #category : #accessing }
GLMTreeMorphModel >> inputText [
	^ inputText ifNil: [inputText := '']
]

{ #category : #accessing }
GLMTreeMorphModel >> inputText: anObject [
	inputText := anObject.
	self doSearchOrFilter
]

{ #category : #accessing }
GLMTreeMorphModel >> inputTextEnabled [
	^ true
]

{ #category : #callbacks }
GLMTreeMorphModel >> keyStroke: aKeyboardEvent from: aTreeMorph [ 
	| action |
	action := self allKeystrokeActions detect: [:a | a shortcut = aKeyboardEvent keyCharacter] ifNone: [nil].
	action ifNotNil: [ self announce: (GLMKeyStroke action: action) ]
]

{ #category : #callbacks }
GLMTreeMorphModel >> menu: menu shifted: b [ 
	"Set up the menu to apply to the receiver's, honoring the #shifted boolean"
	self selectedNode ifNotNil: [:current | current menu: menu shifted: b].
	^ menu
]

{ #category : #accessing }
GLMTreeMorphModel >> nodeModelFor: anObject [
	^ (GLMTreeMorphNodeModel with: anObject) containerTree: self; yourself
]

{ #category : #callbacks }
GLMTreeMorphModel >> onDoubleClick [
	self announcer announce: (GLMTreeMorphStrongSelectionChanged new strongSelectionValue: 
		(self selection ifNotNil: [self selection selectedItemOrItemsOrNil]))
]

{ #category : #actions }
GLMTreeMorphModel >> resetChildrenContents [ 
	self roots do: [:each | each resetContentsRecursively]
]

{ #category : #'search and filtering' }
GLMTreeMorphModel >> resetInput [
	self updateRoots
]

{ #category : #stepping }
GLMTreeMorphModel >> resetStopStepping [
	stopStepping := false.
	self changed.
]

{ #category : #callbacks }
GLMTreeMorphModel >> resetTagsToFilterBy [
	tagsToFilterBy := OrderedCollection new.
	self changed: #rootNodes.
]

{ #category : #accessing }
GLMTreeMorphModel >> rootNodes [
	^self roots
]

{ #category : #accessing }
GLMTreeMorphModel >> roots [
	| filtered |
	roots ifNil: [ self roots: self displayValue ].
	filtered := roots select: [ :each | each shouldBeDisplayed ].
	((self shouldFilterByAmount and: [
		self amountToFilterBy < filtered size]) and: [
			filtered notEmpty]) ifTrue: [
		filtered := filtered first: self amountToFilterBy ].
	^ filtered
]

{ #category : #accessing }
GLMTreeMorphModel >> roots: anObjectOrCollection [
	roots := anObjectOrCollection asOrderedCollection 
				collect: [:each | self nodeModelFor: each ]
]

{ #category : #callbacks }
GLMTreeMorphModel >> selection: aSelection [ 
	self selection = aSelection ifTrue: [ ^ self ].
	super selection: aSelection.
	self announcer announce: (
			GLMTreeMorphSelectionChanged new 
				selectionValue: (aSelection ifNotNil: [aSelection selectedItemOrItemsOrNil]);
				selectionPathValue: self selectionPathItems;
				yourself)
]

{ #category : #accessing }
GLMTreeMorphModel >> selectionPathItems [

	^ self selectedNodePath
		ifNil: [ nil ]
		ifNotNil: [ self selectedNodePath collect: [ :each | each item ] ]
]

{ #category : #stepping }
GLMTreeMorphModel >> shouldDoStep [
	^ self glamourPresentation stepCondition cull: self
]

{ #category : #'search and filtering' }
GLMTreeMorphModel >> shouldFilterByAmount [
	^ self amountToFilterBy notNil and: [self amountToFilterBy isZero not]
]

{ #category : #callbacks }
GLMTreeMorphModel >> shouldFilterByTag: aTag [
	^ self tagsToFilterBy includes: aTag 
]

{ #category : #'search and filtering' }
GLMTreeMorphModel >> shouldFilterByTextInput [
	^ self glamourPresentation allowsFilter and: [ self inputText notEmpty ]
]

{ #category : #stepping }
GLMTreeMorphModel >> shouldStopStepping [

	^ stopStepping ifNil: [ false ]
]

{ #category : #callbacks }
GLMTreeMorphModel >> sortBlock [

	^ columnToSortBy sortBlock 
		ifNotNil: [ :aBlock | aBlock ]
		ifNil: [ 
			columnToSortBy defaultSortBlockForPresentation: self glamourPresentation ]
]

{ #category : #callbacks }
GLMTreeMorphModel >> sortByColumn: aGlamourColumn [

	columnToSortBy := (columnToSortBy == aGlamourColumn 
		ifTrue: [ nil ]
		ifFalse: [ aGlamourColumn ]).
	self updateRoots
]

{ #category : #stepping }
GLMTreeMorphModel >> stepTime [
	^ self glamourPresentation stepTime
]

{ #category : #callbacks }
GLMTreeMorphModel >> tagsToFilterBy [
	^ tagsToFilterBy ifNil: [ tagsToFilterBy := OrderedCollection new ]
]

{ #category : #accessing }
GLMTreeMorphModel >> toggleAmountToFilterBy [
	self amountToFilterBy notNil ifTrue: 
		[ self amountToFilterBy isZero 
			ifTrue: [ amountToFilterBy := self glamourPresentation amountToShow ]
			ifFalse: [ amountToFilterBy := 0 ].
		self changed: #rootNodes ]
]

{ #category : #callbacks }
GLMTreeMorphModel >> toggleFilteringByTag: aTag [ 
	(self shouldFilterByTag: aTag) 
		ifTrue: [self tagsToFilterBy remove: aTag ]
		ifFalse: [self tagsToFilterBy add: aTag].
	self resetChildrenContents.
	self changed: #rootNodes
]

{ #category : #accessing }
GLMTreeMorphModel >> updateRoots [
	roots := nil.
	self changed: #rootNodes
]

{ #category : #'drag and drop' }
GLMTreeMorphModel >> wantsDroppedMorph: transferMorph event: evt inMorph: listMorph [ 
	| targetItem targetItemMorph |
	(transferMorph isKindOf: TransferMorph) ifFalse: [ ^ false ].
	transferMorph dragTransferType = self dragTransferType ifFalse: [ ^ false ].
	
	targetItemMorph := (listMorph scrollerSubMorphFromPoint: evt position).
	targetItemMorph ifNotNil: [
		targetItem := targetItemMorph complexContents item.	
		^ self glamourPresentation allow: transferMorph passenger droppedOnItem: targetItem ].
	self flag: 'we should ask the whole presentation if it wants to receive a dropped object'.
	^ false
]

{ #category : #stepping }
GLMTreeMorphModel >> wantsSteps [
	^ self shouldStopStepping not and: [
		self glamourPresentation wantsSteps ]
]

{ #category : #announcements }
GLMTreeMorphModel >> when: anAnnouncement do: aBlock [
	self announcer when: anAnnouncement do: aBlock 
]
