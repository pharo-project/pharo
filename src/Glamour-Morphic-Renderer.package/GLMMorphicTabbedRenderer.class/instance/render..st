rendering
render: aPresentation
	| presentations index |
	presentations := aPresentation matchingPresentations.
	presentations isEmpty
		ifTrue: [ ^ GLMMorphic emptyMorph ].
	(presentations size = 1 and: [ presentations first hasTitle not ])
		ifTrue: [ ^ self renderWithTitleOrNil: presentations first ].
	tabs := LazyTabGroupMorph new.
	tabs
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		cornerStyle: (self theme tabGroupCornerStyleIn: nil);
		font: self theme buttonFont.
	tabs announcer
		when: LazyTabPageChanged
		do: [ :ann | 
			aPresentation announcer
				suspendAllWhile: [ aPresentation pane lastActivePresentation: (aPresentation matchingPresentations at: ann pageIndex) ] ].
	presentations
		do: [ :each | 
			tabs
				addLazyPage: [ self renderObject: each ]
				label: (self titleOrIconOf: each in: tabs)
				toolbar: (self renderToolbarForPresentation: each inMorph: tabs)
				action: (self labelActionBrickFor: each) ].
	index := 1.
	presentations
		withIndexDo: [ :each :i | 
			(aPresentation pane isLastActivePresentation: each)
				ifTrue: [ index := i ] ].
	tabs announcer suspendAllWhile: [ tabs selectedPageIndex: index ].
	aPresentation when: GLMContextChanged send: #actOnContextChanged: to: self.
	aPresentation when: GLMPresentationUpdated send: #actOnPresentationUpdated: to: self.
	aPresentation when: GLMPresentationToolbarUpdated send: #actOnPresentationToolbarUpdated: to: self.
	^ tabs