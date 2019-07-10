Class {
	#name : #DropListAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
DropListAdapterTest >> classToTest [

	^ DropListPresenter 
]

{ #category : #accessing }
DropListAdapterTest >> initializeTestedInstance [ 
	
	super initializeTestedInstance.
	presenter items: #(10 20 30).
]

{ #category : #accessing }
DropListAdapterTest >> testAddingIconsShowTheIcon [

	#(pharo add book) do: [ :each |
		self presenter addItemLabeled: each do: [] icon: (self iconNamed: each)].

	self presenter selectedItem: #pharo.

	self assert: self adapter selectedIndexes equals: #(4).
	backendForTest assertIcon: self adapter displayedIcon equals: (self iconNamed: #pharo). 
]

{ #category : #accessing }
DropListAdapterTest >> testDoesNotShowIcons [

	backendForTest assertIcon: self adapter displayedIcon equals: nil. 
]

{ #category : #accessing }
DropListAdapterTest >> testRemovingIconsAfterAddingThem [

	#(pharo add book) do: [ :each |
		self presenter addItemLabeled: each do: [] icon: (self iconNamed: each)].

	self presenter selectedItem: nil.
	self presenter model removeAll.

	#(pharo add book) do: [ :each |
		self presenter addItemLabeled: each do: [] icon: nil].

	self assert: self adapter selectedIndexes equals: #(1).
	backendForTest assertIcon: self adapter displayedIcon equals: nil. 
]

{ #category : #accessing }
DropListAdapterTest >> testResetingSelectionResetsSelection [

	self presenter selectedItem: 10.
	
	self presenter resetSelection.

	self assert: self presenter selectedItem equals: nil.
	self assert: self adapter selectedIndexes isEmpty.
	
]

{ #category : #accessing }
DropListAdapterTest >> testSelectingASecondElementSelectsIt [

	self presenter selectedItem: 10.
	self presenter selectedItem: 20.

	self assert: self presenter selectedItem equals: 20.
	self assert: self adapter selectedIndexes equals: #(2).
	
]

{ #category : #accessing }
DropListAdapterTest >> testSelectingAnElementInWidgetUpdatePresenter [

	self adapter selectIndex: 2.

	self assert: self presenter selectedItem equals: 20.
	self assert: self adapter selectedIndexes equals: #(2).
	
]

{ #category : #accessing }
DropListAdapterTest >> testSelectingAnElementSelectsIt [

	self presenter selectedItem: 10.

	self assert: self presenter selectedItem equals: 10.
	self assert: self adapter selectedIndexes equals: #(1).
	
]

{ #category : #accessing }
DropListAdapterTest >> testSelectionExecutesBlock [

	| selected |

	self presenter dropListItems: (#(10 20 30) 
		collect: [ :e | DropListItem on: e do: [ selected := e ] ]).

	self presenter selectedItem: 20.
	self assert: selected equals: 20	
]

{ #category : #accessing }
DropListAdapterTest >> testSelectionStartsSelectingFirst [

	self assert: self presenter selectedItem equals: 10.
	self assert: self adapter selectedIndexes equals: #(1).
	
]

{ #category : #accessing }
DropListAdapterTest >> testSettingNilResetsGeneratesEvent [
	| changed |
	self presenter selectedItem: 10.
	self presenter whenSelectionChangedDo: [ changed := true ].
	self presenter selectedItem: nil.
	self assert: changed
]

{ #category : #accessing }
DropListAdapterTest >> testSettingNilResetsSelection [

	self presenter selectedItem: 10.
	self presenter selectedItem: nil.

	self assert: self presenter selectedItem equals: nil.
	self assert: self adapter selectedIndexes isEmpty.
	
]
