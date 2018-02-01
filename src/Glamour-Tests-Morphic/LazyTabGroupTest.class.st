Class {
	#name : #LazyTabGroupTest,
	#superclass : #TestCase,
	#instVars : [
		'window',
		'tabs'
	],
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #running }
LazyTabGroupTest >> setUp [
	super setUp.
	tabs := LazyTabGroupMorph new.
	tabs addLazyPage: (PanelMorph new fillStyle: (SolidFillStyle color: Color red)) label: 'one'.
	tabs addLazyPage: (PanelMorph new fillStyle: (SolidFillStyle color: Color yellow)) label: (GLMLabelBrick new text: 'two').
	window := tabs openInWindow.

]

{ #category : #running }
LazyTabGroupTest >> tearDown [
	window ifNotNil: [ window delete ].
	super tearDown
]

{ #category : #tests }
LazyTabGroupTest >> testIsRendered [
	self assert: tabs pages first isRendered not.
	tabs selectedPageIndex: 1.
	self assert: tabs pages first isRendered.
	self assert: tabs pages second isRendered not
]

{ #category : #tests }
LazyTabGroupTest >> testPageChangedAnnouncement [
	| announcement page |
	announcement := 0.
	tabs announcer
		when: LazyTabPageChanged
		do: [ :ann | 
			announcement := announcement + 1.
			page := ann page ].
	tabs selectedPageIndex: 1.
	self assert: announcement equals: 1.
	self assert: (page isKindOf: LazyTabPage)
]

{ #category : #tests }
LazyTabGroupTest >> testPageRemovedAnnouncement [
	| announcement |
	announcement := 0.
	tabs announcer when: LazyTabPageRemoved do: [ :ann | announcement := announcement + 1 ].
	tabs announcer when: LazyTabPageChanged do: [ :ann | announcement := announcement + 1 ].
	tabs removePage: tabs pages first.
	self assert: announcement equals: 1
]

{ #category : #tests }
LazyTabGroupTest >> testRemoveAllPages [
	tabs removePage: tabs pages first.
	tabs removePage: tabs pages first.
	self assert: tabs pages isEmpty.
	self assert: tabs contentMorph submorphs isEmpty
]

{ #category : #tests }
LazyTabGroupTest >> testRemovePage [
	tabs removePage: tabs pages first.
	self assert: tabs pages size equals: 1
]

{ #category : #tests }
LazyTabGroupTest >> testSimpleCreation [
	self assert: tabs pages size equals: 2
]
