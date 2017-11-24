Class {
	#name : #GLMMorphicAccumulatorRenderer,
	#superclass : #GLMMorphicBrowserRenderer,
	#instVars : [
		'tabs'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicAccumulatorRenderer >> actOnPaneAdded: ann [ 
	tabs 
		addLazyPage: (GLMMorphicPaneWithoutTitleRenderer new render: ann pane) 
		label: (self titleOrIconOf: ann pane presentations in: tabs)
		toolbar: nil
		collapsable: true.
	tabs selectedPageIndex: tabs pages size
]

{ #category : #rendering }
GLMMorphicAccumulatorRenderer >> actOnPaneSelected: anAnnouncement [
	tabs selectedPageIndex: anAnnouncement position
]

{ #category : #private }
GLMMorphicAccumulatorRenderer >> removePage: aPage [
	| removedPageIndex |
	removedPageIndex := self pages indexOf: aPage.
	self announcer suspendAllWhile: [ super removePage: aPage ].
	self announcer announce: 
		(LazyTabPageRemoved new 
			tabs: self; 
			page: aPage; 
			pageIndex: removedPageIndex;
			newIndex: self tabSelectorMorph selectedIndex).
	self pages isEmpty 
		ifTrue: [ self contentMorph removeAllMorphs ]
]

{ #category : #rendering }
GLMMorphicAccumulatorRenderer >> render: aBrowser [
	aBrowser panes isEmpty
		ifTrue: [ ^ GLMMorphic emptyMorph ].
	tabs := LazyTabGroupMorph new.
	tabs
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		cornerStyle: (self theme tabGroupCornerStyleIn: nil);
		font: self theme labelFont.
	tabs announcer
		when: LazyTabPageChanged
		do: [ :ann | aBrowser activeEntity: ((aBrowser panes at: ann pageIndex) port: #entity) value ].
	tabs announcer
		when: LazyTabPageRemoved
		do: [ :ann | 
			aBrowser removePaneIndex: ann pageIndex.
			aBrowser panes isEmpty
				ifTrue: [ aBrowser activeEntity: nil ]
				ifFalse: [ aBrowser activeEntity: ((aBrowser panes at: ann newIndex) port: #entity) value ] ].
	aBrowser panes
		do: [ :each | 
			tabs
				addLazyPage: [ GLMMorphicPaneWithoutTitleRenderer new render: each ]
				label: (self titleOrIconOf: each presentations in: tabs)
				toolbar: (self renderToolbarForPresentation: each presentations inMorph: tabs)
				collapsable: true ].
	aBrowser when: GLMPaneSelected send: #actOnPaneSelected: to: self.
	aBrowser when: GLMPaneAdded send: #actOnPaneAdded: to: self.
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self.
	tabs selectedPageIndex: tabs pages size.
	^ tabs
]
