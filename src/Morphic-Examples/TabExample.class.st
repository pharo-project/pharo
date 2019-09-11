"
I am an example of how to:
	- instanciate tabs (#tabs)
	- use a tab manager
	
Nothing really complicated here.

self open
"
Class {
	#name : #TabExample,
	#superclass : #Model,
	#instVars : [
		'manager',
		'listModel',
		'window'
	],
	#category : #'Morphic-Examples-Tabs'
}

{ #category : #'instance creation' }
TabExample class >> open [
	<script>
	^ self new open
]

{ #category : #private }
TabExample >> addTabs [

	self tabs do: [ :tab | manager addTab: tab ]
]

{ #category : #'tabs creation' }
TabExample >> blueMorphTab [
	| tab |
	tab := TabMorph
		label: 'Blue Morph'
		icon: (self iconNamed: #smallBackIcon)
		morph: Morph new.
	tab
		actions:
			{(TabAction
				action: [ tab morph color: Color random ]
				icon: (self iconNamed: #smallConfigurationIcon)
				label: 'Randomize color')}.
	tab
		closeable: false;
		menu: [ :menu | menu add: 'Fubu' target: self selector: #halt ].
	^ tab
]

{ #category : #private }
TabExample >> delete [

	window ifNotNil: [ window delete ]
]

{ #category : #'tabs creation' }
TabExample >> freshListTab [
	<haltOrBreakpointForTesting>
	^ (TabMorph
		label: 'Fresh List'
		icon: nil
		retrievingBlock: [ 1 second asDelay wait.
			SpListPresenter new
				items: (1 to: 50) asOrderedCollection;
				buildWithSpec ]
		actions:
		{(TabAction
				action: [ self halt ]
				icon: (self iconNamed: #smallConfigurationIcon)
				label: 'Halt!')})
		menu: [ :menu | menu add: 'Fubu' target: self selector: #halt ];
		yourself
]

{ #category : #initialization }
TabExample >> initialize [

	super initialize.

	manager := TabManagerMorph new.
	manager
		when: #tabManagerDeleted send: #delete to: self.
	listModel := SpListPresenter new.
	listModel items: ((1 to: 1000) asOrderedCollection)
]

{ #category : #'instance creation' }
TabExample >> open [
	window := manager openInWindow.
	window extent: 400 @ 300.

	self addTabs
]

{ #category : #'tabs creation' }
TabExample >> redMorphTab [
	^ (LockableTabMorph
		label: 'Red Morph'
		icon: (self iconNamed: #smallBackIcon)
		morph:
		(Morph new
		color: Color red;
		yourself))
		closeable: false;
		yourself
]

{ #category : #'tabs creation' }
TabExample >> storedListTab [
	^ TabMorph
		label: 'Stored List Model'
		icon: (self iconNamed: #smallHelpIcon)
		retrievingBlock: [ listModel buildWithSpec ]
]

{ #category : #private }
TabExample >> tabs [
	
	^ OrderedCollection new
		add: self storedListTab;
		add: self freshListTab;
		add: self blueMorphTab;
		add: self redMorphTab;
		yourself
]

{ #category : #accessing }
TabExample >> window [

	^ window
]
