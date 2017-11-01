Class {
	#name : #GLMCompositePresentationMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : 'Glamour-Tests-Morphic'
}

{ #category : #private }
GLMCompositePresentationMorphicTest >> compositeMorph [
	^ window submorphs last submorphs last submorphs last
]

{ #category : #private }
GLMCompositePresentationMorphicTest >> compositePanel [
	|composite|
	composite := self compositeMorph.
	self assert: (composite isKindOf: PanelMorph).
	self assert: (composite submorphs allSatisfy: [:morph | morph isKindOf: ExpanderMorph]).
	^ composite

]

{ #category : #private }
GLMCompositePresentationMorphicTest >> compositeTabGroup [
	|composite|
	composite := self compositeMorph.
	self assert: (composite isKindOf: LazyTabGroupMorph).
	^ composite

]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testAccordion [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a accordionArrangement.
					a list title: 'List'.
					a text title: 'Text' ].
	window := browser openOn: 42.
	self assert: self compositePanel submorphs notEmpty.
	self assert: self compositePanel submorphs size equals: 2.
	self assert: self compositePanel submorphs first class equals: ExpanderMorph.
	self assert: self compositePanel submorphs last class equals: ExpanderMorph
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testAccordionExpanded [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a | 
		a accordionArrangement.
		a list title: 'List'.
		a text title: 'Text'].
	window := browser openOn: 42.
	self assert: self compositePanel submorphs first expanded.
	self assert: self compositePanel submorphs last expanded not.
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testAccordionWithTitle [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a accordionArrangement.
					a title: 'title'.
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs notEmpty.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph class equals: GLMPanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs first class equals: ExpanderMorph
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testFinder [
	| finder |
	finder := GLMFinder new.
	finder
		show: [ :a | 
			a title: 'something'.
			a stackedArrangement.
			a list.
			a text ].
	self assert: finder transmission transmissionStrategy  presentations size equals: 2.
	finder startOn: (1 to: 42).
	self assert: finder panes first presentations size equals: 2.
	self assert: (finder panes first presentations arrangement isKindOf: GLMStackedVerticallyArrangement).
	window := finder open.
	self find: LazyTabGroupMorph in: window.
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testOneStackedVerticallyWithTitle [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a stackedVerticallyArrangement.
					a title: 'title'.
					a list ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs notEmpty.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph class equals: GLMPanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs size equals: 1.
	self assert: self compositeTabGroup contentMorph submorphs first class equals: GLMPanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs size equals: 1.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs first class equals: LazyTabGroupMorph
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testOneTabWithActions [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list act: #inspect entitled: 'Inspect' ].
	window := browser openOn: 42.
	self assert: self compositeMorph class equals: GLMPanelMorph.
	self assert: self compositeMorph submorphs size equals: 2
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testStackedVertically [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a stackedVerticallyArrangement.
					a list title: 'list'.
					a text title: 'text' ].
	window := browser openOn: 42.
	self assert: self compositeMorph class equals: GLMPanelMorph.
	self assert: self compositeMorph submorphs size equals: 2.
	self assert: self compositeMorph submorphs size equals: 2.
	self assert: self compositeMorph submorphs first class equals: LazyTabGroupMorph
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testStackedVerticallyWithTitle [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a stackedVerticallyArrangement.
					a title: 'title'.
					a list title: 'list'.
					a text title: 'text' ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs notEmpty.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph class equals: GLMPanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs size equals: 1.
	self assert: self compositeTabGroup contentMorph submorphs first class equals: GLMPanelMorph.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs size equals: 2.
	self assert: self compositeTabGroup contentMorph submorphs first submorphs first class equals: LazyTabGroupMorph
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testTabs [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup pages size equals: 2
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testTabsLastActivePresentation [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list.
					a text ].
	window := browser openOn: 42.
	self compositeTabGroup selectedPageIndex: 2.
	self assert: browser panes first lastActivePresentation notNil.
	self assert: (browser panes first lastActivePresentation isKindOf: GLMRubricTextPresentation)
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testTabsWithActions [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list act: #inspect entitled: 'Inspect'.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup toolbarMorph submorphs notEmpty.
	self compositeTabGroup selectedPageIndex: 2.
	self assert: self compositeTabGroup toolbarMorph submorphs isEmpty.
	self compositeTabGroup selectedPageIndex: 1.
	self assert: self compositeTabGroup toolbarMorph submorphs notEmpty
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testTabsWithTitle [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a title: 'title'.
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup submorphs first class equals: GLMPanelMorph.
	self assert: self compositeTabGroup submorphs first submorphs size equals: 1.
	self assert: self compositeTabGroup submorphs first submorphs first class equals: LazyTabGroupMorph.
	self assert: self compositeTabGroup submorphs first submorphs first pages size equals: 2
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testTabsWithTitleIconWithoutTitle [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a titleIcon: GLMUIThemeExtraIcons glamorousBrowse.
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup submorphs size equals: 2.
	self assert: self compositeTabGroup submorphs first class equals: GLMPanelMorph.
	self assert: self compositeTabGroup submorphs first submorphs size equals: 1.
	self assert: self compositeTabGroup submorphs first submorphs first class equals: LazyTabGroupMorph.
	self assert: self compositeTabGroup submorphs first submorphs first pages size equals: 2
]

{ #category : #tests }
GLMCompositePresentationMorphicTest >> testTabsWithUpdate [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup pages size equals: 2.
	self shouldnt: [browser panes first presentations update]  raise: Error
]
