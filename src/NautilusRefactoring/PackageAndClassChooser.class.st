"
I am a widget to select classes and packages from a tree.
"
Class {
	#name : #PackageAndClassChooser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'tree',
		'searchField',
		'searchPattern',
		'updateProcess'
	],
	#category : #'NautilusRefactoring-Utilities'
}

{ #category : #specs }
PackageAndClassChooser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :c |
			c 
				add: #searchField height: 25;
				add: #tree
		];
		yourself
]

{ #category : #'accessing - tree' }
PackageAndClassChooser >> childrenFor: aPackageOrClass [
	| result name |
	name := aPackageOrClass name.
	aPackageOrClass isBehavior 
		ifTrue: [ ^ #() ].
	
	aPackageOrClass isTrait 
		ifTrue: [ ^ #() ].
	result := Set new.
	RPackageOrganizer default packagesDo: [ :each | | tempName |
			tempName := each name.
			(tempName size > name size 
			and: [ (tempName at: name size + 1) = $- 
			and: [ (tempName beginsWith: name) ]])
				ifTrue: [ result add: each ]]. 
			
	result := result asSortedCollection: [ :a :b | a name <= b name ].
	result addAll: aPackageOrClass definedClasses.
	^ result
					
]

{ #category : #actions }
PackageAndClassChooser >> choose [
	^ self chooseModalTo: World
]

{ #category : #actions }
PackageAndClassChooser >> chooseModalTo: parentWindow [
	"Open this this package class chooser as a dialog modal to the given window"
	| dialog result |
	
	dialog := self openDialogWithSpec.
	result := nil.
	dialog 
		okAction: [ result := self selectedItems ];
		cancelAction: [ result := nil ].
	parentWindow openModal: dialog window.
	^ result
]

{ #category : #'accessing - tree' }
PackageAndClassChooser >> filteredRoots [
	|  roots |
	roots := RPackageOrganizer default packages 
		select: [ :package | searchPattern search: package name ].
	roots := roots asSortedCollection: [ :a :b | a name <= b name ].
	Smalltalk allClassesAndTraitsDo: [ :class | 
		(searchPattern search: class name)
			ifTrue: [ roots add: class ]].
	^ roots
]

{ #category : #testing }
PackageAndClassChooser >> hasSearch [
	^ self searchPattern isNil not
]

{ #category : #'accessing - tree' }
PackageAndClassChooser >> iconFor: anRPackageOrClass [
	anRPackageOrClass isBehavior
		ifTrue: [ ^ self iconNamed: #classIcon ].
	RPackageOrganizer default
		packageNamed: anRPackageOrClass name
		ifAbsent: [ ^ self iconNamed: #emptyPackageIcon ].
	^ self iconNamed: #packageIcon
]

{ #category : #initialization }
PackageAndClassChooser >> initializeWidgets [

	self
		setFocus;
		update.
]

{ #category : #'accessing - tree' }
PackageAndClassChooser >> labelFor: aPackageOrClass [ 

	self hasSearch
		ifTrue: [ ^ aPackageOrClass name ].
	
	^ (aPackageOrClass name copyAfterLast: $-)
		ifEmpty: [ aPackageOrClass name ]
]

{ #category : #'accessing - tree' }
PackageAndClassChooser >> roots [
	| rootNames organizer|
	
	self hasSearch 
		ifTrue: [ ^ self filteredRoots ].
		
	organizer := RPackageOrganizer default.
	rootNames := (organizer packageNames collect: [ :name |
		name copyUpTo: $- ]) asSet asSortedCollection.
	
	^ rootNames collect: [  :name|
		organizer 
			packageNamed: name
			ifAbsent: [ RPackage named: name ]]
]

{ #category : #accessing }
PackageAndClassChooser >> searchField [
	^ searchField ifNil: [ 
		searchField := SearchMorph new
			model: self;
			updateSelector: #searchUpdate:;
			on: (Character arrowDown asKeyCombination) do: [ :arg :arg2 :keystroke | 
				self tree spec instance handleKeystroke: keystroke ];
			yourself.
		searchField := searchField asSpecAdapter ]
]

{ #category : #accessing }
PackageAndClassChooser >> searchPattern [
	^ searchPattern
]

{ #category : #updating }
PackageAndClassChooser >> searchUpdate: aString [
	searchPattern := nil.
	
	aString isEmptyOrNil ifFalse: [
		searchPattern := [aString asRegexIgnoringCase] on: RegexSyntaxError do: [ aString ]].
	
	self update.
]

{ #category : #accessing }
PackageAndClassChooser >> selectedItem [
	^ self tree selectedItem
]

{ #category : #accessing }
PackageAndClassChooser >> selectedItems [
	^ self tree selectedItems
]

{ #category : #'accessing - tree' }
PackageAndClassChooser >> selectionChanged [
	"Synchronize the selected items"
]

{ #category : #initialization }
PackageAndClassChooser >> setFocus [
	
	self focusOrder
		add: self searchField;
		add: self tree.
	
]

{ #category : #accessing }
PackageAndClassChooser >> tree [
	^ tree ifNil: [ 
		tree := self instantiate: TreePresenter.
		tree
			isCheckList: true;
			multiSelection: true;
			autoMultiSelection: true;
		"	rootNodeHolder: [ :item | 
				TreeNodePresenter with: item model: tree ];"
	
			displayBlock:  [ :aPackageOrClass |
				self labelFor: aPackageOrClass ];
			childrenBlock: [ :aPackageOrClass | 
				self childrenFor: aPackageOrClass ];
			iconBlock: [ :treeNode | 
				self iconFor: treeNode content ];
			
			whenSelectedItemChanged: [ :selection | self selectionChanged ];
			doubleClick: [
				 Smalltalk tools browser openOnPackage: tree selectedItem ]]
]

{ #category : #updating }
PackageAndClassChooser >> update [
	updateProcess ifNotNil: [ updateProcess terminate ].
	updateProcess := [ |roots| 
			roots := self roots.
			UIManager default defer: [ self tree roots: roots ].
		] fork.
]
