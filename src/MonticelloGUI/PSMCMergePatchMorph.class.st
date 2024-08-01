"
Display Monticello merge patchs
"
Class {
	#name : 'PSMCMergePatchMorph',
	#superclass : 'DiffChangeMorph',
	#instVars : [
		'changeTree',
		'selectedChangeWrapper'
	],
	#category : 'MonticelloGUI-Diff',
	#package : 'MonticelloGUI',
	#tag : 'Diff'
}

{ #category : 'icons' }
PSMCMergePatchMorph class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallForward
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> allConflicts [
	"Answer all conflicts in the model."

	^self model select: [:op | op isConflict]
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> allConflictsMenu [
	"Answer the all conflicts menu."

	| menu |
	menu := self morphicUIManager newMenuIn: self for: self.
	menu
		addToggle: 'Keep current version of all' translated
		target: self
		selector: #keepAllCurrentVersion
		getStateSelector: nil
		enablementSelector: #hasAnyNonKeptConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallBackIcon);
		keyText: 'Shift+Cmd+c'.
	menu
		addToggle: 'Use incoming version of all' translated
		target: self
		selector: #useAllIncomingVersion
		getStateSelector: nil
		enablementSelector: #hasAnyNonIncomingConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Shift+Cmd+g'.
	menu
		addToggle: 'Mark all as conflict' translated
		target: self
		selector: #markAllAsConflict
		getStateSelector: nil
		enablementSelector: #hasAnyNonConflictConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancel);
		keyText: 'Shift+Cmd+x'.
	^ menu
]

{ #category : 'actions' }
PSMCMergePatchMorph >> browseClass [
	"Browse the class of the selected item."

	^ Smalltalk tools browser
		openOnClass: self selectedChangeWrapper actualClass
		selector: self selectedMessageName
]

{ #category : 'actions' }
PSMCMergePatchMorph >> browseImplementors [
	"Browse the method implementors."

	self systemNavigation
		browseAllImplementorsOf: (self selectedMessageName ifNil: [^self])
]

{ #category : 'actions' }
PSMCMergePatchMorph >> browseSenders [
	"Browse the method senders."

	self systemNavigation
		browseAllSendersOf: (self selectedMessageName ifNil: [^self])
]

{ #category : 'actions' }
PSMCMergePatchMorph >> browseVersions [
	"Browse the method versions."

	Smalltalk tools versionBrowser
			browseVersionsForClass: self selectedChangeWrapper actualClass
			selector: self selectedMessageName
]

{ #category : 'initialization' }
PSMCMergePatchMorph >> buildUI [
	|descriptionHeight|
	self
		diffMorph: self newDiffMorph;
		changeTree: self newChangeTreeMorph;
		descriptionMorph: self newDescriptionMorph.
	descriptionHeight := self descriptionMorph minExtent y.
	self
		changeProportionalLayout;
		addMorph: self descriptionMorph
		fullFrame: ((0@0 corner: 1@0) asLayoutFrame leftOffset: ProportionalSplitterMorph splitterWidth; bottomOffset: descriptionHeight);
		addMorph: self changeTree
		fullFrame: ((0@0 corner: 0.3@1) asLayoutFrame topLeftOffset: ProportionalSplitterMorph splitterWidth @ descriptionHeight);
		addMorph: self diffMorph
		fullFrame: ((0.3@0 corner: 1@1) asLayoutFrame topLeftOffset: ProportionalSplitterMorph splitterWidth @ descriptionHeight);
		addPaneSplitters.
	self updateDescriptionFillStyle: self paneColor.
	self onAnnouncement: MorphOpened do: [ self expandChangeTree ]
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> changeTree [
	"Answer the value of changeTree"

	^ changeTree
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> changeTree: anObject [
	"Set the value of changeTree"

	changeTree := anObject
]

{ #category : 'private' }
PSMCMergePatchMorph >> changeTreeKey: aCharacter [
	"Handle change tree key strokes."

	(aCharacter == $b and: [self selectionHasActualClass]) ifTrue: [
		^self browseClass].
	(aCharacter == $v and: [self selectionIsMethodChange]) ifTrue: [
		^self browseVersions].
	(aCharacter == $m and: [self selectionIsMethodChange]) ifTrue: [
		^self browseImplementors].
	(aCharacter == $n and: [self selectionIsMethodChange]) ifTrue: [
		^self browseSenders].
	(aCharacter == $a and: [ self selectionIsNotNil]) ifTrue: [
		^ self toggleApplySelectedChange.
		].
	(aCharacter == $c and: [self selectionIsNotNil]) ifTrue: [
		^self keepCurrentVersion].
	(aCharacter == $g and: [self selectionIsNotNil]) ifTrue: [
		^self useIncomingVersion].
	(aCharacter == $x and: [self selectionIsNotNil]) ifTrue: [
		^self markAsConflict].
	(aCharacter == $f and: [self notAllConflictsResolved]) ifTrue: [
		^self selectNextConflict].

	(aCharacter == $C and: [self hasAnyNonKeptConflicts]) ifTrue: [
		^self keepAllCurrentVersion].
	(aCharacter == $G and: [self hasAnyNonIncomingConflicts]) ifTrue: [
		^self useAllIncomingVersion].
	(aCharacter == $X and: [self hasAnyNonConflictConflicts]) ifTrue: [
		^self markAllAsConflict]
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> changes [
	"Answer the changes tree roots."

	^ PSMCChangesGrouper group: self model
]

{ #category : 'private' }
PSMCMergePatchMorph >> changesMenu: m [
	"Answer the changes menu."

	| menu |
	menu := self newMenu
		addTitle: 'Changes'
		icon: (self iconNamed: #smallCopy).
	menu
		addToggle: 'Browse class...' translated
		target: self
		selector: #browseClass
		getStateSelector: nil
		enablementSelector: #selectionHasActualClass.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallWindow);
		keyText: 'Cmd+b'.
	menu addLine.
	menu
		addToggle: 'Versions...' translated
		target: self
		selector: #browseVersions
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallJustifiedIcon);
		keyText: 'Cmd+v'.
	menu
		addToggle: 'Senders...' translated
		target: self
		selector: #browseSenders
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Cmd+n'.
	menu
		addToggle: 'Implementors...' translated
		target: self
		selector: #browseImplementors
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallDoIt);
		keyText: 'Cmd+m'.
	menu
		addToggle: 'Install incoming version' translated
		target: self
		selector: #loadMethodSelection
		getStateSelector: nil
		enablementSelector: #selectionIsMethodChange.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallUpdate).
	menu
		addToggle: 'Revert change' translated
		target: self
		selector: #revertSelectedChange
		getStateSelector: nil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancel).
	menu
		add: 'Expand All' translated
		target: self model
		selector: #expandAll.
	menu
		add: 'Collapse All' translated
		target: self model
		selector: #collapseAll.
	menu addLine.
	menu
		addToggle: 'Apply this change' translated
		target: self
		selector: #toggleApplySelectedChange
		getStateSelector: #selectionIsRemoteChosen
		enablementSelector: #selectionIsNotNil.
	menu
		addToggle: 'Keep current version' translated
		target: self
		selector: #keepCurrentVersion
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallBackIcon);
		keyText: 'Cmd+c'.
	menu
		addToggle: 'Use incoming version' translated
		target: self
		selector: #useIncomingVersion
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Cmd+g'.
	menu
		addToggle: 'Mark as conflict' translated
		target: self
		selector: #markAsConflict
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancel);
		keyText: 'Cmd+x'.
	menu
		addToggle: 'All conflicts' translated
		target: self
		selector: nil
		getStateSelector: nil
		enablementSelector: #hasConflicts.
	menu lastItem
		font: self theme menuFont;
		subMenu: self allConflictsMenu.
	menu addLine.
	menu
		addToggle: 'Select next conflict' translated
		target: self
		selector: #selectNextConflict
		getStateSelector: nil
		enablementSelector: #notAllConflictsResolved.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallRightFlush);
		keyText: 'Cmd+f'.
	^ menu
]

{ #category : 'actions' }
PSMCMergePatchMorph >> compositeText [
	"Answer the composite text based on the selection state
	of the joins."

	^self diffMorph compositeText
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> defaultTitle [
	"Answer the default title label for the receiver."

	^'Changes' translated
]

{ #category : 'hooks' }
PSMCMergePatchMorph >> diffMorphClass [
	"Answer a the class to use for a new diff morph."

	^MergeDiffMorph
]

{ #category : 'updating' }
PSMCMergePatchMorph >> expandChangeTree [
	self flag: #TODO. "We should only expand the nodes wich have partial changes, if a class was added or removed there is no need to show the complete tree expanded"
	self changeTree expandAll
]

{ #category : 'testing' }
PSMCMergePatchMorph >> hasAnyNonConflictConflicts [
	"Answer whether there are any conflicts not marked as a conflict."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved]
]

{ #category : 'testing' }
PSMCMergePatchMorph >> hasAnyNonIncomingConflicts [
	"Answer whether there are any conflicts not marked as incoming."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved not or: [conflict localChosen]]
]

{ #category : 'testing' }
PSMCMergePatchMorph >> hasAnyNonKeptConflicts [
	"Answer whether there are any conflicts not marked as keep."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved not or: [conflict remoteChosen]]
]

{ #category : 'testing' }
PSMCMergePatchMorph >> hasConflicts [
	"Answer whether there are any conflicts."

	^self allConflicts notEmpty
]

{ #category : 'actions' }
PSMCMergePatchMorph >> keepAllCurrentVersion [
	"Mark all conflicts as local."

	self allConflicts do: [:c | c chooseLocal].
	self changed: #changes.
	self updateSource
]

{ #category : 'actions' }
PSMCMergePatchMorph >> keepCurrentVersion [
	"Mark the conflict as local."

	self selectedChangeWrapper chooseLocal.
	self changed: #changes.
	self updateSource
]

{ #category : 'menus' }
PSMCMergePatchMorph >> loadMethodSelection [
	"Install the selected change"

	self selectedChange ifNil: [ ^self ].
	self selectedChange definition load
]

{ #category : 'actions' }
PSMCMergePatchMorph >> markAllAsConflict [
	"Mark all conflicts as unresolved."

	self allConflicts do: [:c | c clearChoice].
	self changed: #changes.
	self updateSource
]

{ #category : 'actions' }
PSMCMergePatchMorph >> markAsConflict [
	"Mark the operation as unresolved conflict."

	self selectedChangeWrapper operation beConflict.
	self changed: #changes.
	self updateSource
]

{ #category : 'initialization' }
PSMCMergePatchMorph >> model [

	^ super model ifNil: [ #() ]
]

{ #category : 'initialization' }
PSMCMergePatchMorph >> model: aCollection [
	"Set the model and update the window."

	super model: aCollection.
	self changed: #changes
]

{ #category : 'instance creation' }
PSMCMergePatchMorph >> newChangeTreeMorph [
	"Answer a new morph for the tree of changes."

	^(self
		newTreeFor: self
		list: #changes
		selected: #selectedChangeWrapper
		changeSelected: #selectedChangeWrapper:)
		getMenuSelector: #changesMenu:;
		keystrokeActionSelector: #changeTreeKey:
]

{ #category : 'actions' }
PSMCMergePatchMorph >> nextConflict [

	"Answer the next (unresolved) conflict or, nil if none."

	| coll current index |

	current := self selectedChangeWrapper.
	index := current ifNil: [ 0 ] ifNotNil: [ self model indexOf: current item ].
	coll := ( self model copyFrom: index + 1 to: self model size )
		, ( self model copyFrom: 1 to: index ).
	^ coll detect: [ :item | item isConflict and: [ item isResolved not ] ] ifNone: [ nil ]
]

{ #category : 'testing' }
PSMCMergePatchMorph >> notAllConflictsResolved [
	"Answer whether any conflicts are unresolved."

	^self model anySatisfy: [:item | item isConflict and: [item isResolved not]]
]

{ #category : 'private' }
PSMCMergePatchMorph >> removeSelection [
	"unselect current selection"
	self selectedChangeWrapper: nil.
	self changed: #changes.
	self updateSource
]

{ #category : 'private' }
PSMCMergePatchMorph >> revertChange: aChange [
	"Revert the change"

	| loader |
	loader := MCPackageLoader new.
	aChange inverse applyTo: loader.
	loader load
]

{ #category : 'private' }
PSMCMergePatchMorph >> revertSelectedChange [
	"Revert the selected change"
	| change |
	change := self selectedChange.
	change ifNil: [ ^ self ].
	self revertChange: change.
	self model remove: change.
	self removeSelection
]

{ #category : 'actions' }
PSMCMergePatchMorph >> selectNextConflict [
	"Select the next conflict after the current selection, if any."

	|next op def path|
	next := self nextConflict ifNil: [^self].
	op := next operation.
	def := next targetDefinition ifNil: [next baseDefinition].
	path := {#changes. #openPath}.
	def isMethodDefinition ifTrue: [path := path, {def fullClassName}].
	path := path, {op shortSummary}.
	self changed: path
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> selectedChange [
	"Answer the selected change."

	^(self selectedChangeWrapper ifNil: [^nil]) operation
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> selectedChangeWrapper [
	"Answer the selected change."

	^selectedChangeWrapper
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> selectedChangeWrapper: aWrapper [
	"Set the selected change."

	selectedChangeWrapper := aWrapper.
	self
		changed: #selectedChangeWrapper;
		updateSource;
		changed: #compositeText
]

{ #category : 'accessing' }
PSMCMergePatchMorph >> selectedMessageName [
	"Answer the method selector or nil if no method change
	is selected.."

	^self selectionIsMethodChange
		ifTrue: [self selectedChange definition selector]
]

{ #category : 'private' }
PSMCMergePatchMorph >> selectionHasActualClass [
	"Answer whether the currently selected change has an actual
	class in the image."

	^self selectedChangeWrapper
		ifNil: [false]
		ifNotNil: [:w | w actualClass isNotNil]
]

{ #category : 'private' }
PSMCMergePatchMorph >> selectionIsMethodChange [
	"Answer whether the currently selected change is for a method."

	^self selectedChange isNotNil and: [
		self selectedChange definition isMethodDefinition and: [
			self selectedChange definition isLoadable ] ]
]

{ #category : 'actions' }
PSMCMergePatchMorph >> selectionIsNotNil [

	^ self selectedChange isNotNil
]

{ #category : 'testing' }
PSMCMergePatchMorph >> selectionIsRemoteChosen [
	self selectedChange ifNil: [ ^ false].
	^ self selectedChange remoteChosen
]

{ #category : 'actions' }
PSMCMergePatchMorph >> toggleApplySelectedChange [

	self selectedChange remoteChosen
		ifTrue: [ self selectedChange chooseLocal ]
		ifFalse: [ self selectedChange chooseRemote ].
	self changed: #changes.
	self updateSource
]

{ #category : 'updating' }
PSMCMergePatchMorph >> update: aspect [
	"A join has probably changed its selection state."

	super update: aspect.
	aspect == #selectedDifferences
		ifTrue: [self changed: #compositeText]
]

{ #category : 'updating' }
PSMCMergePatchMorph >> updateSource [
	"Update the source difference morph."

	| sel |
	sel := self selectedChangeWrapper.
	self diffMorph allowJoinClicks: (sel isNotNil and: [ sel isConflict and: [ sel operation isModification ] ]).
	self selectedChange
		ifNil: [ self diffMorph from: '' to: '' ]
		ifNotNil: [ :sel2 |
			self diffMorph from: sel2 diffFromSource to: sel2 diffToSource contextClass: (sel2 isClassPatch
					 ifTrue: [ nil ]
					 ifFalse: [ sel2 targetClass ]) ].
	(sel isNil or: [ sel isConflict not ]) ifTrue: [ ^ self ].
	sel localChosen
		ifTrue: [ self diffMorph indicateSrc ]
		ifFalse: [ self diffMorph indicateDst ]
]

{ #category : 'actions' }
PSMCMergePatchMorph >> useAllIncomingVersion [
	"Mark all conflicts as remote."

	self allConflicts do: [:c | c chooseRemote].
	self changed: #changes.
	self updateSource
]

{ #category : 'actions' }
PSMCMergePatchMorph >> useIncomingVersion [
	"Mark the conflict as remote."

	self selectedChangeWrapper chooseRemote.
	self changed: #changes.
	self updateSource
]
