Class {
	#name : #GLMMorphicStackerRenderer,
	#superclass : #GLMMorphicBrowserRenderer,
	#category : #Glamour-Morphic-Renderer
}

{ #category : #rendering }
GLMMorphicStackerRenderer >> render: aBrowser [
	| tabs |
	aBrowser panes isEmpty
		ifTrue: [ ^ GLMMorphic emptyMorph ].
	tabs := LazyTabGroupMorph new.
	tabs
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		cornerStyle: (self theme tabGroupCornerStyleIn: nil);
		font: self theme labelFont.
	aBrowser panes
		do: [ :each | 
			self flag: 'add toolbar'.
			tabs
				addLazyPage: [ 
					GLMMorphicPaneWithoutTitleRenderer new 
						renderer: self renderer;
						render: each ]
				label: (self titleOrIconOf: each presentations in: tabs)
				toolbar: (self renderToolbarForPresentation: each presentations inMorph: tabs) ].
	tabs selectedPageIndex: 1.
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self.
	^ tabs
]
