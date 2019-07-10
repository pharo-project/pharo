Class {
	#name : #GLMTreeMorphNodeModel,
	#superclass : #MorphTreeNodeModel,
	#instVars : [
		'contents',
		'containerTree'
	],
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #comparing }
GLMTreeMorphNodeModel >> = anObject [
	"This is needed to handle the update of the whole list while still preserving the selection"
	^ self species = anObject species and: [ self item = anObject item ]
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> allContents [
	^ self childrenItems
		collect: [ :d | 
			(self class with: d)
				parentNode: self;
				containerTree: self containerTree;
				yourself ]
]

{ #category : #callbacks }
GLMTreeMorphNodeModel >> buttonForTag: each filter: filter [
	| button |

	button := GLMMorphic 
		togglingButtonLabelled: each
		pressed: (self containerTree shouldFilterByTag: filter)
		style: self containerTree glamourPresentation tagsStyle.
	button
		target: self;
		actionSelector: #toggleFilteringByTag:;
		arguments: (Array with: filter).
	^ button
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> childrenItems [
	^ (self containerTree glamourPresentation childrenValueOf: self item atLevel: self path size) ifNil: [OrderedCollection new]
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> containerTree [
	^ containerTree
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> containerTree: anObject [
	containerTree := anObject
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> contents [
	^ contents ifNil: [ contents := self allContents select: [ :each | each shouldBeDisplayed ] ]
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> displayText [
	^ (UITheme current 
		newTextIn: self currentWorld
		text: (self containerTree glamourPresentation formatedDisplayValueOf: self item))
			backgroundColor: self textBackgroundColor;
			yourself
]

{ #category : #callbacks }
GLMTreeMorphNodeModel >> elementColumn [
	| rowElements |
	
	rowElements := OrderedCollection new
		add: self displayText;
		addAll: (self tagMorphsFrom: self containerTree glamourPresentation);
		yourself.
	
	^ (UITheme current newRowIn: self currentWorld for: rowElements).
]

{ #category : #testing }
GLMTreeMorphNodeModel >> enabled [
	^ true
]

{ #category : #menu }
GLMTreeMorphNodeModel >> executeMenuAction: anAction [
	self containerTree announce: (GLMMenuItemSelected action: anAction)
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> expandParentPath [
"	self openItemPath: anArray"
	self containerTree expandNodePath: self path
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> helpText [
	^ nil
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> icon [
	^ self containerTree glamourPresentation iconFor: self item
]

{ #category : #menu }
GLMTreeMorphNodeModel >> keyStroke: aCharacter from: aTreeMorph [ 
	^ false

]

{ #category : #menu }
GLMTreeMorphNodeModel >> menu: aMenuMorph shifted: b [ 
	| subMenus targetMenuMorph subMenu allActions|
	subMenus := Dictionary new.
	allActions := self containerTree allMenuActions.
	allActions do: [ :action |	 
		targetMenuMorph := action category notNil 
			ifTrue: [ subMenus at: action category ifAbsentPut: [ 
					subMenu :=  MenuMorph new.
					aMenuMorph add: action category subMenu: subMenu.
					subMenu ] ]
			ifFalse: [ aMenuMorph ].
		(targetMenuMorph 
			add: action title
			target: self
			selector: #executeMenuAction:
			argument: action)
			enabled: (action isEnabledOn: self containerTree glamourPresentation);
			icon:  action icon;
			keyText: (action hasShortcut 
				ifTrue: [ action shortcutAsString ]
				ifFalse: [ nil ]).
			(action separatorAfter and: [ allActions last ~= action ]) ifTrue: [ targetMenuMorph addLine ] 	
				 ].
	^ aMenuMorph
]

{ #category : #callbacks }
GLMTreeMorphNodeModel >> modified: aGlamourColumn with: aText [

	aGlamourColumn modifiedBlock
		cull: aText
		cull: self item
		cull: self containerTree glamourPresentation
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> parentNode [
	^ parentNode
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> parentNode: aNode [
	parentNode := aNode
	
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> path [
	"returns the list of nodes to the root"
	^ self pathIn: OrderedCollection new.
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> pathIn: aCollection [ 
	self parentNode ifNotNil: [
		(aCollection includes: self parentNode)
			ifFalse: [self parentNode pathIn: aCollection]].
	aCollection add: self.
	^ aCollection
]

{ #category : #printing }
GLMTreeMorphNodeModel >> printOn: aStream [
	aStream nextPutAll: 'a NodeModel with '.
	aStream nextPutAll: self item printString.
]

{ #category : #actions }
GLMTreeMorphNodeModel >> resetContents [
	contents := nil
]

{ #category : #actions }
GLMTreeMorphNodeModel >> resetContentsRecursively [ 
	self resetContents.
	self allContents do: [:each | each resetContentsRecursively]
]

{ #category : #callbacks }
GLMTreeMorphNodeModel >> rowMorphForColumn: aGlamourColumn [
	| rowElements contentMorph tagMorphs |
		
	aGlamourColumn modifiedBlock
		ifNil: [
			contentMorph := (self containerTree glamourPresentation 
			column: aGlamourColumn
			valueFor: self item) asMorph asReadOnlyMorph ]
		ifNotNil: [
			contentMorph := GLMEditableLabelBrick new
				cancelOnEscape;
				cancelOnFocusLost;
				text: (self valueFor: aGlamourColumn);
				hSpaceFill;
				with: [ :b | b editBrick rubricBackgroundColor: b themer rubricBackgroundColor ];
				when: #onEditMode do: [ self containerTree forceStopStepping ];
				when: #onReadMode do: [ self containerTree resetStopStepping ];
				when: #onSingleClicked do: [  :evt :brick | brick isReadMode ifTrue: [
						(brick ownerThatIsA: GLMPaginatedResizableMorphTreeMorph) 
							listManager mouseUp: evt on: brick owner owner owner.

						"self containerTree 
							deselectAll;
							selectNodePath: self path" ] ];
				when: #onAccepted do: [ self modified: aGlamourColumn with: contentMorph text ];
				yourself.
			
			contentMorph := contentMorph asMorph
				hSpaceFill;
				vShrinkWrap ].
	
	tagMorphs := self tagMorphsFrom: aGlamourColumn.
	tagMorphs ifEmpty: [ ^ contentMorph ].
	
	rowElements := OrderedCollection new
		add: contentMorph;
		addAll: tagMorphs;
		yourself.			
	^ (UITheme current newRowIn: self currentWorld for: rowElements)
]

{ #category : #actions }
GLMTreeMorphNodeModel >> selectItem [

	self containerTree explicitlySelectItem: self item
]

{ #category : #testing }
GLMTreeMorphNodeModel >> shouldBeDisplayed [
	^ self shouldBeDisplayedByTags and: [ self shouldBeDisplayedByText ]
]

{ #category : #testing }
GLMTreeMorphNodeModel >> shouldBeDisplayedByTags [
	| nodeTags |
	self flag: 'it looks like this method does not really get called for children'.
"	self item mooseName = #'org::easymock' ifTrue: [self haltOnce ].
"	self containerTree tagsToFilterBy isEmpty ifTrue: [ ^ true ].
	self withContentsDo: [ :node | 
		nodeTags := self containerTree glamourPresentation tagsFor: node item to: #filter.
		(self containerTree tagsToFilterBy allSatisfy: [ :each | nodeTags includes: each ])
			ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #testing }
GLMTreeMorphNodeModel >> shouldBeDisplayedByText [
	self containerTree shouldFilterByTextInput ifFalse: [ ^ true ].
	self withContentsDo: 
		[ :node | 
		self flag: 'this should be delegated to the presentation
		and glamourValue should be used to include the entity ports'.
		(self containerTree glamourPresentation filterStrategy 
			value: self containerTree inputText asString
			value: node item) == true ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #callbacks }
GLMTreeMorphNodeModel >> tagMorphsFrom: aTagProvider [
	| tagMorphs tags tagsFilter |
	tagMorphs := OrderedCollection new.
	tags:= self containerTree glamourPresentation tagsFor: self item fromProvider: aTagProvider to: #show.
	tagsFilter := self containerTree glamourPresentation tagsFor: self item fromProvider: aTagProvider to: #filter.
	tags withIndexDo:[ :each :index | 
			tagMorphs addLast: (self buttonForTag: each 
									  filter: (tagsFilter at: index ifAbsentPut:[ each ])) ].
	^ tagMorphs
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> textBackgroundColor [
	^ self containerTree glamourPresentation textBackgroundColorFor: self item
]

{ #category : #callbacks }
GLMTreeMorphNodeModel >> toggleFilteringByTag: aTag [ 
	self containerTree toggleFilteringByTag: aTag
]

{ #category : #accessing }
GLMTreeMorphNodeModel >> valueFor: aGlamourColumn [

	^ self containerTree glamourPresentation 
			column: aGlamourColumn
			valueFor: self item
]

{ #category : #enumerating }
GLMTreeMorphNodeModel >> withContentsDo: aBlock [
	aBlock value: self.
	self containerTree glamourPresentation allowsRecursiveSearch ifTrue: [  
		self contents do: [:each | each withContentsDo: aBlock ] ]
]
