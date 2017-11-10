Class {
	#name : #GLMMorphicTabbedRenderer,
	#superclass : #GLMMorphicCompositeRenderer,
	#instVars : [
		'tabs'
	],
	#category : #Glamour-Morphic-Renderer
}

{ #category : #rendering }
GLMMorphicTabbedRenderer >> actOnContextChanged: ann [
	ann property = #activePresentation ifTrue: [ 
		tabs announcer suspendAllWhile: [ tabs selectedPageIndex: (ann presentation matchingPresentations indexOf: ann value) ] ]
]

{ #category : #rendering }
GLMMorphicTabbedRenderer >> actOnPresentationToolbarUpdated: ann [
	| presentations |

	presentations := ann presentation matchingPresentations.
	tabs pages size to: 1 by: -1 do: [ :i | (tabs pages at: i) toolbar: GLMMorphic emptyMorph ].	
	presentations withIndexDo: [ :each :i | 
		tabs updatePageAtIndex: i
			withLabel: (self titleOrIconOf: each in: tabs)
			toolbar: (self renderToolbarForPresentation: each inMorph: tabs)
			action: (self labelActionMorphFor: each)]
]

{ #category : #rendering }
GLMMorphicTabbedRenderer >> actOnPresentationUpdated: ann [
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
]

{ #category : #rendering }
GLMMorphicTabbedRenderer >> render: aPresentation [
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
]
