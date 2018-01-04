"
A DualChangeSorterApplication is a dual change sorter based on spec.

DualChangeSorter shares the model between its two subcomponents (ChangeSorterApplication).
ChangeSorterModel defines the application logic.

"
Class {
	#name : #DualChangeSorterApplication,
	#superclass : #ComposablePresenter,
	#instVars : [
		'model',
		'changeSorterLeft',
		'changeSorterRight',
		'isRefreshing'
	],
	#category : #'Spec-Tools-ChangeSorter'
}

{ #category : #menu }
DualChangeSorterApplication class >> changesListMenu: aBuilder [
	<contextMenu>
	<dualChangeSorteChangesListMenu>
	
	| target src dest |

	self flag: #todo. "I do not like how I'm getting the parameters (an Array), but looks the faster way now :(
	And I need src and dest parameters because it is called in two sides of change sorter"
	target := aBuilder model first.
	src := aBuilder model second.
	dest := aBuilder model third. 
	
	src selectedChangeSet ifNil:[ ^self ].
	
	(aBuilder item: #'Copy all to the other change set')
		keyText: 'c';
		action: [ target copyAllFrom: src to: dest ].
	(aBuilder item: #'Submerge into the other change set')
		action: [ target submerge: src into: dest ].
	(aBuilder item: #'Subtract the other change set')
		keyText: '-';
		action: [ target subtractFrom: src to: dest ];
		withSeparatorAfter.
]

{ #category : #menu }
DualChangeSorterApplication class >> classListMenu: aBuilder [
	<contextMenu>
	<dualChangeSorterClassListMenu>
	
	| target src dest |
	
	self flag: #todo. "I do not like how I'm getting the parameters (an Array), but looks the faster way now :(
	And I need src and dest parameters because it is called in two sides of change sorter"
	target := aBuilder model first.
	src := aBuilder model second.
	dest := aBuilder model third. 
	
	(aBuilder item: #'Copy class to the other change set')
		keyText: 'c';
		action: [ target copyClassFrom: src to: dest ].
	(aBuilder item: #'Move class to the other change set')
		action: [ target moveClassFrom: src to: dest ].

]

{ #category : #specs }
DualChangeSorterApplication class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
			newRow: [:r |
					r 
						add: #changeSorterLeft;
						addSplitter;
						add: #changeSorterRight ]
]

{ #category : #menu }
DualChangeSorterApplication class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder group: #SystemChanges)
		parent: #Tools;
		order: 0.51;
		with: [ (aBuilder item: #'Change Sorter')
				action: [ self open ];
				help: 'Examine the different change set of the image.';
				icon: self taskbarIcon ].
	aBuilder withSeparatorAfter
]

{ #category : #menu }
DualChangeSorterApplication class >> messageListMenu: aBuilder [
	<contextMenu>
	<dualChangeSorterMessageListMenu>
	
	| target src dest |
	
	self flag: #todo. "I do not like how I'm getting the parameters (an Array), but looks the faster way now :(
	And I need src and dest parameters because it is called in two sides of change sorter"
	target := aBuilder model first.
	src := aBuilder model second.
	dest := aBuilder model third. 
	
	(aBuilder item: #'Copy method to the other change set')
		action: [ target copyMethodFrom: src to: dest ].
	(aBuilder item: #'Move method to the other change set')
		action: [ target moveMethodFrom: src to: dest ];
		withSeparatorAfter.

]

{ #category : #specs }
DualChangeSorterApplication class >> open [
	self new openWithSpec
]

{ #category : #'tools-registry' }
DualChangeSorterApplication class >> registerToolsOn: registry [

	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #changeSorter
]

{ #category : #icons }
DualChangeSorterApplication class >> taskbarIconName [
	^ #changeSorterIcon
]

{ #category : #specs }
DualChangeSorterApplication class >> title [

	^'Dual Change Sorter'
]

{ #category : #accessing }
DualChangeSorterApplication >> changeSorterLeft [

	^ changeSorterLeft
]

{ #category : #accessing }
DualChangeSorterApplication >> changeSorterRight [

	^ changeSorterRight
]

{ #category : #menu }
DualChangeSorterApplication >> changesMenu: menu shifted: shifted from: sourcePanel to: destinationPanel [
	| menuModel menuDest |

	menuModel := MenuPresenter new.
	menu addTitle: 'Change Set'.
	sourcePanel changeSetMenu1: menu shifted: shifted.
	menuDest := MenuPresenter new
		fromSpec:
			(PragmaMenuBuilder
				pragmaKeyword: 'dualChangeSorteChangesListMenu'
				model:
					{self.
					sourcePanel.
					destinationPanel}) menuSpec.
	menuDest menuGroups do: [ :each | menuModel addMenuGroup: each ].
	menu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	sourcePanel changeSetMenu2: menu shifted: shifted.
	^ menu
]

{ #category : #'menu - change set' }
DualChangeSorterApplication >> checkThatSidesDiffer: escapeBlock [
	"If the change sets on both sides of the dual sorter are the same, put up an error message and escape via escapeBlock, else proceed happily"

	changeSorterLeft selectedChangeSet == changeSorterRight selectedChangeSet
		ifFalse: [ ^ self ].
	self
		inform:
			'This command requires that the
change sets selected on the two
sides of the change sorter *not*
be the same.'.
	^ escapeBlock value
]

{ #category : #menu }
DualChangeSorterApplication >> classMenu: menu shifted: shifted from: sourcePanel to: destinationPanel [
	"Fill aMenu with items appropriate for the class list"
	| menuModel menuDest |
	
	menuModel := MenuPresenter new.
	sourcePanel selectedClass ifNil: [ ^ nil ].
	sourcePanel classMenu: menu shifted: shifted.
	menuDest := MenuPresenter new fromSpec: (PragmaMenuBuilder
		pragmaKeyword: 'dualChangeSorterClassListMenu'
		model: {self. sourcePanel. destinationPanel}) 
		menuSpec.
	menuDest menuGroups do: [ :each | menuModel addMenuGroup: each ].
	menu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^ menu
]

{ #category : #'menu - change set' }
DualChangeSorterApplication >> copyAllFrom: src to: dest [

	self checkThatSidesDiffer: [ ^ self ].
	self model copyAllChangesFrom: (src selectedChangeSet) to: (dest selectedChangeSet).
	dest updateClassesList.
]

{ #category : #'menu - class' }
DualChangeSorterApplication >> copyClassFrom: src to: dest [
	"Place these changes in the other changeSet also"

	| otherChangeSet |
	self checkThatSidesDiffer: [ ^ self ].
	self okToChange ifFalse: [ ^ self inform: 'Can''t discard edits.' ].
	src selectedClass ifNil: [ ^ self inform: 'Selected class is nil.' ].
	otherChangeSet := dest selectedChangeSet.
	
	self model copyClass: src selectedClass from: src selectedChangeSet to: otherChangeSet.
	
	dest setSelectedChangeSet: otherChangeSet.
]

{ #category : #'menu - method' }
DualChangeSorterApplication >> copyMethodFrom: src to: dest [
	"Place this change in the other changeSet also"	
	| other |
	
	self checkThatSidesDiffer: [^ self].
	other := dest selectedChangeSet.
	
	src selectedSelector ifNotNil: [:selector || class |
		class := src selectedClass.
		
		self model 
			copySelector: selector 
			inClass: class 
			from: src selectedChangeSet 
			to: other.
			
		dest updateClassesListAndMessagesList ].
]

{ #category : #initialization }
DualChangeSorterApplication >> initialExtent [

	^  900@530
]

{ #category : #initialization }
DualChangeSorterApplication >> initialize [

	super initialize.
	model := ChangeSorterModel new.
	isRefreshing := false.
	SystemAnnouncer uniqueInstance weak 
		when: CurrentChangeSetChanged 
		send: #updateTitle 
		to: self 
]

{ #category : #initialization }
DualChangeSorterApplication >> initializePresenter [

	changeSorterLeft 	whenChangesListChanges: [ self refeshedChangeSet: changeSorterRight ].
	changeSorterRight	whenChangesListChanges: [ self refeshedChangeSet: changeSorterLeft ].


]

{ #category : #initialization }
DualChangeSorterApplication >> initializeWidgets [

	changeSorterLeft := self instantiate: ChangeSorterApplication.
	changeSorterRight := self instantiate: ChangeSorterApplication.
		
	self menusRegistration.
	self shortcutsRegistration.
	self setFocusOrder
]

{ #category : #initialization }
DualChangeSorterApplication >> menusRegistration [

	self menusRegistrationFrom: changeSorterLeft to: changeSorterRight.
	self menusRegistrationFrom: changeSorterRight to: changeSorterLeft
]

{ #category : #menu }
DualChangeSorterApplication >> menusRegistrationFrom: src to: dest [
	
	src changesMenu: [:menu :shifted | self changesMenu: menu shifted: shifted from: src to: dest ].
	src classesMenu: [:menu :shifted | self classMenu: menu shifted: shifted from: src to: dest ].
	src methodsMenu: [:menu :shifted | self messageMenu: menu shifted: shifted from: src to: dest ].
]

{ #category : #menu }
DualChangeSorterApplication >> messageMenu: menu shifted: shifted from: sourcePanel to: destinationPanel [
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"
	
	| menuModel menuDual |
	
	menuModel := MenuPresenter new.
	sourcePanel selectedSelector ifNil: [ ^ nil ].
	sourcePanel messageMenu: menu shifted: shifted.
	menuDual := MenuPresenter new fromSpec: (PragmaMenuBuilder 
		pragmaKeyword: 'dualChangeSorterMessageListMenu'
		model: { self. sourcePanel. destinationPanel }) 
		menuSpec.
	menuDual menuGroups do: [ :each | menuModel addMenuGroup: each ].
	menu addAllMorphs: (menuModel buildWithSpec: #popup) items.
	^ menu
]

{ #category : #accessing }
DualChangeSorterApplication >> model [

	^ model
]

{ #category : #'menu - class' }
DualChangeSorterApplication >> moveClassFrom: src to: dest [

	self checkThatSidesDiffer: [^ self].
	(self okToChange and: [ src selectedClass notNil]) ifFalse: [ ^ self ].

	self copyClassFrom: src to: dest.
	src forgetClass
]

{ #category : #'menu - method' }
DualChangeSorterApplication >> moveMethodFrom: src to: dest [

	self copyMethodFrom: src to: dest.
	src forgetMessage.
	src updateClassesListAndMessagesList.
]

{ #category : #event }
DualChangeSorterApplication >> refeshedChangeSet: changeSet [
	isRefreshing
		ifTrue: [ ^ self ].
	isRefreshing := true.
	changeSet updateChangesList.
	isRefreshing := false
]

{ #category : #initialization }
DualChangeSorterApplication >> setFocusOrder [

	self focusOrder
		add: changeSorterLeft;
		add: changeSorterRight.
]

{ #category : #initialization }
DualChangeSorterApplication >> shortcutsRegistration [

	self shortcutsRegistrationFrom: changeSorterLeft to: changeSorterRight.
	self shortcutsRegistrationFrom: changeSorterRight to: changeSorterLeft.
]

{ #category : #initialization }
DualChangeSorterApplication >> shortcutsRegistrationFrom: src to: dest [
	
	src changesPresenterOn: $c meta do: [ self copyAllFrom: src to: dest ].
	src changesPresenterOn: $- meta do: [ self subtractFrom: src to: dest ]. 
]

{ #category : #'menu - change set' }
DualChangeSorterApplication >> submergeFrom: src into: dest [
	"Copy the contents of the receiver to the other side, then remove the receiver -- all after checking that all is well."
	| source destination index |

	source := src selectedChangeSet.
	destination := dest selectedChangeSet.
	index := src selectedChangeSetIndex.

	(self model submerge: source into: destination) ifFalse: [ ^ self ].
	
	src updateChangesList.
	src setSelectedChangeSetIndex: index.
]

{ #category : #'menu - change set' }
DualChangeSorterApplication >> subtractFrom: src to: dest [
	"Subtract the changes found on the other side from the requesting side."
	| source destination |
	source := src selectedChangeSet.
	destination := dest selectedChangeSet.
	
	self checkThatSidesDiffer: [^ self].
	
	self model subtractFrom: source to: destination.
	
	changeSorterRight setSelectedChangeSet: source.
]

{ #category : #event }
DualChangeSorterApplication >> title [

	^ super title, ' on: ', self model currentChangeSet name. 
]
