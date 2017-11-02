Class {
	#name : #GLMMorphicAccordionRenderer,
	#superclass : #GLMMorphicCompositeRenderer,
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicAccordionRenderer >> render: aPresentation [
	| presentations container expander |
	presentations := aPresentation matchingPresentations.
	presentations isEmpty
		ifTrue: [ ^ GLMMorphic emptyMorph ].
	presentations size = 1
		ifTrue: [ ^ self renderWithToolbar: presentations first ].
	container := GLMMorphic containerMorph.
	container changeTableLayout.
	presentations
		do: [ :each | 
			expander := self theme
				newExpanderIn: container
				label: (self titleOrIconOf: each in: container)
				forAll: {(self renderWithToolbar: each)}.
			container addMorphBack: expander.
			(aPresentation pane lastActivePresentation notNil
				and: [ each title = aPresentation pane lastActivePresentation title ])
				ifTrue: [ expander expanded: true ] ].
	(aPresentation pane lastActivePresentation isNil and: [ container submorphs notEmpty ])
		ifTrue: [ container submorphs first expanded: true ].
	^ container
]
