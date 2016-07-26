rendering
actOnPresentationUpdated: ann
	| index presentations |
	presentations := ann presentation matchingPresentations.
	tabs pages size to: 1 by: -1 do: [ :i | tabs removePageIndex: i ].	
	presentations do: [ :each | 
		tabs 
			addLazyPage: [ self renderObject: each ]
			label: (self titleOrIconOf: each in: tabs)
			toolbar: (self renderToolbarForPresentation: each inMorph: tabs )
			action: (self labelActionBrickFor: each) ].
	index := 1.
	presentations withIndexDo: [ :each :i | 
		ann presentation pane lastActivePresentation notNil and: [ 
			each title = ann presentation pane lastActivePresentation title ifTrue: [ 
				index := i ] ] ].
	tabs selectedPageIndex: index.