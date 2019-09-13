"
Description
--------------------

I am a demo showing how to use Spec lists.
"
Class {
	#name : #SpDemoListsPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'list1',
		'list2',
		'list3',
		'label',
		'multiLabel'
	],
	#category : #'Spec2-Examples-Demo-Lists'
}

{ #category : #specs }
SpDemoListsPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add:
					(SpBoxLayout newVertical
						add: 'Menu' withConstraints: [ :constraints | constraints height: self labelHeight ];
						add: #list1;
						yourself);
				add:
					(SpBoxLayout newVertical
						add: 'Multi selection/Sorting' withConstraints: [ :constraints | constraints height: self labelHeight ];
						add: #list2;
						yourself);
				add:
					(SpBoxLayout newVertical
						add: 'Icons/Filter' withConstraints: [ :constraints | constraints height: self labelHeight ];
						add: #list3;
						yourself);
				yourself);
		add:
			(SpBoxLayout newHorizontal
				add: #multiLabel;
				add: #label;
				yourself)
			withConstraints: [ :constraints | constraints height: self labelHeight ];
		yourself
]

{ #category : #initialization }
SpDemoListsPresenter >> connectPresenters [
	list1
		whenSelectionChangedDo: [ :selection | 
			[ :elem | 
			label
				label:
					(elem
						ifNil: [ 'Deselection from list 1' ]
						ifNotNil: [ 'Selection from list 1: ' , elem asString ]) ]
				cull: selection selectedItem ].

	list2
		whenSelectionChangedDo: [ :selection | 
			[ :elems | 
			multiLabel
				label:
					(elems
						ifEmpty: [ 'Not multi selection (from list 2 only)' ]
						ifNotEmpty: [ 'Multi-selection from list 2: ' , elems asString ]) ]
				cull: selection selectedItems ].
	list3
		whenSelectionChangedDo: [ :selection | 
			[ :elem | 
			label
				label:
					(elem
						ifNil: [ 'Deselection from list 3' ]
						ifNotNil: [ 'Selection from list 3: ' , elem asString ]) ]
				cull: selection selectedItem ]
]

{ #category : #defaults }
SpDemoListsPresenter >> iconFor: class [
	((class includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ])) and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest ifTrue: [ ^ self iconNamed: #testGreenIcon ].
			class hasFailedTest ifTrue: [ ^ self iconNamed: #testYellowIcon ].
			class hasErrorTest ifTrue: [ ^ self iconNamed: #testRedIcon ].
			^ self iconNamed: #testNotRunIcon ].
	class organization classComment ifEmpty: [ ^ self iconNamed: #uncommentedClassIcon ].
	((class includesBehavior: (Smalltalk globals at: #TestCase ifAbsent: [ false ])) and: [ class isAbstract not ])
		ifTrue: [ class hasPassedTest ifTrue: [ ^ self iconNamed: #testGreenIcon ].
			class hasFailedTest ifTrue: [ ^ self iconNamed: #testYellowIcon ].
			class hasErrorTest ifTrue: [ ^ self iconNamed: #testRedIcon ].
			^ self iconNamed: #testNotRunIcon ].
	class organization classComment ifEmpty: [ ^ self iconNamed: #uncommentedClassIcon ].
	^ class systemIcon
]

{ #category : #initialization }
SpDemoListsPresenter >> initializePresenters [
	list1 := self newList.
	list2 := self newList.
	list3 := self newList.
	label := self newLabel.
	multiLabel := self newLabel.

	list1
		items: self class environment allClasses;
		contextMenu: self list1Menu.
	list2
		items: self class environment allClasses;
		sortingBlock: [ :a :b | a asString size < b asString size ];
		beMultipleSelection.
	list3
		items: self class environment allClasses;
		itemFilter: [ :item :string | item asString includesSubstring: string ];
		icons: [ :elem | self iconFor: elem ].
	label label: 'No selection'.
	multiLabel label: 'No multi selection (from list 2 only)'
]

{ #category : #accessing }
SpDemoListsPresenter >> label [
	^ label
]

{ #category : #accessing }
SpDemoListsPresenter >> list1 [
	^ list1
]

{ #category : #menu }
SpDemoListsPresenter >> list1Menu [
	^ self newMenu
		title: 'Context menu example';
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Print in transcript';
						shortcut: $p meta;
						description: 'Print the selected class on Transcript';
						action: [ list1 selectedItem traceCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Browse';
						iconName: #open;
						action: [ list1 selectedItem browse ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Print infos';
						subMenu:
							(self newMenu
								addGroup: [ :aSubGroup | 
									aSubGroup
										addItem: [ :item | 
											item
												name: 'Print number of methods';
												action: [ list1 selectedItem methods size traceCr ] ];
										addItem: [ :item | 
											item
												name: 'Print number of variables';
												action: [ list1 selectedItem instanceVariables size traceCr ] ] ]) ] ];
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Item showing only if item a odd number of letters';
						visibleIf: [ list1 selectedItem printString size odd ];
						action: [ list1 selectedItem traceCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Disabled in second group';
						disable;
						action: [ list1 selectedItem traceCr ] ];
				addItem: [ :anItem | 
					anItem
						name: 'Disabled if class begins with A';
						enabled: [ list1 selectedItem printString first ~= $A ];
						action: [ list1 selectedItem traceCr ] ] ]
]

{ #category : #accessing }
SpDemoListsPresenter >> list2 [
	^ list2
]

{ #category : #accessing }
SpDemoListsPresenter >> list3 [
	^ list3
]

{ #category : #accessing }
SpDemoListsPresenter >> multiLabel [
	^ multiLabel
]
