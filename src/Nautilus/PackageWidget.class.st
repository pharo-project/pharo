"
PackageWidget is the basic implementation of a wiget managing Packages and Groups.

Note however that the design is ugly because it mixes UI and Model. 
All the code in this hierarchy is conceptually bogus and should be rewritten.
"
Class {
	#name : #PackageWidget,
	#superclass : #AbstractWidget,
	#instVars : [
		'packagesList',
		'packagesSelection',
		'groupsSelection'
	],
	#category : #'Nautilus-Widgets'
}

{ #category : #protocol }
PackageWidget >> buildContents [
	packagesList buildContents
]

{ #category : #'item creation' }
PackageWidget >> buildPackagesList [
	| treeModel |
	treeModel := PackageTreeModel new
		model: self model;
		yourself.
	packagesList := (MorphTreeMorph on: treeModel)
		multiSelection: true;
		dropEnabled: true;
		getMenuSelector: #menu:shifted:;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		doubleClickSelector: #doubleClickEvent:onNodeMorph:;
		yourself.
	treeModel announcer
		when: MorphTreeSelectionChanged
		do: [ :ann | self model selectedFromPackageGroupView: ann selection ].
	packagesList buildContents.
	self model selectedPackage ifNotNil: [ self model listSelectItem: self model selectedPackage ].
	^ packagesList
]

{ #category : #protocol }
PackageWidget >> deselectAll [
	packagesList model deselectAll
]

{ #category : #'event handling' }
PackageWidget >> doubleClick [
	self selectedNode ifNotNil: [ :n | n doubleClick ]
]

{ #category : #protocol }
PackageWidget >> enabled: aTrue [ 
	packagesList model enabled: aTrue
]

{ #category : #protocol }
PackageWidget >> expandAllFromNode: aPackageTreePackageNodeModel [ 
	packagesList model expandAllFromNode: aPackageTreePackageNodeModel 
]

{ #category : #protocol }
PackageWidget >> hasFocus [
	^ packagesList hasKeyboardFocus 
]

{ #category : #initialization }
PackageWidget >> initialize [

	super initialize.

	packagesList := nil.
	packagesSelection := Dictionary new.
	groupsSelection := Dictionary new.
]

{ #category : #private }
PackageWidget >> packagesList [
	^ packagesList 
]

{ #category : #private }
PackageWidget >> packagesSelection [
	^ packagesSelection 
]

{ #category : #protocol }
PackageWidget >> removeShortcuts: aSymbol [
	self model removeShortcuts: aSymbol from: packagesList
]

{ #category : #protocol }
PackageWidget >> resetGroupsSelection [
	groupsSelection removeAll
]

{ #category : #protocol }
PackageWidget >> resetPackagesSelection [
	packagesSelection removeAll
]

{ #category : #protocol }
PackageWidget >> scrollSelectionIntoView [
	packagesList scrollSelectionIntoView
]

{ #category : #protocol }
PackageWidget >> selectGroup: aGroup [
	groupsSelection at: aGroup put: true
]

{ #category : #protocol }
PackageWidget >> selectItems: aCollection [ 
	packagesList model selectItems: aCollection 
]

{ #category : #protocol }
PackageWidget >> selectPackage: aPackage [
	self packagesSelection at: aPackage put: true
]

{ #category : #protocol }
PackageWidget >> selectedGroup [
	^ self selectedGroups ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]
]

{ #category : #protocol }
PackageWidget >> selectedGroups [

	| associations |
	associations := groupsSelection associations select: [:assoc | assoc value ].
	associations := associations collect: [:assoc | assoc key ].
	^ associations select: [:each | each notNil ]
]

{ #category : #protocol }
PackageWidget >> selectedPackage [

	^ self selectedPackages ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]
]

{ #category : #protocol }
PackageWidget >> selectedPackageGroup [
	^ self selectedPackage ifNil: [ self selectedGroup ]
]

{ #category : #protocol }
PackageWidget >> selectedPackages [
	| associations |
	associations := packagesSelection associations select: [ :assoc | assoc value ].
	associations := associations collect: [ :assoc | assoc key ].
	^ associations select: [ :each | each notNil ]
]

{ #category : #protocol }
PackageWidget >> setShortcuts: aSymbol [
	self model setShortcuts: aSymbol to: packagesList
]

{ #category : #protocol }
PackageWidget >> takeKeyboardFocus [
	packagesList takeKeyboardFocus 
]

{ #category : #updating }
PackageWidget >> updateContentsWithPreviouslyExpanded: aCollection [ 
	packagesList updateContentsWithPreviouslyExpanded: aCollection
]

{ #category : #protocol }
PackageWidget >> updateList [
	packagesList model updateList
]

{ #category : #protocol }
PackageWidget >> updatePackageView [
	packagesList model changed:#rootNodes
]
