rendering
render: aPresentation
	textModel := self modelFor: aPresentation.
	textModel primarySelectionInterval: (aPresentation primarySelectionInterval 
		ifNil:[ aPresentation selectionInterval ifNil: [1 to: 0] ]).
	textMorph := self morph.
	aPresentation text: textMorph text.
	aPresentation withLineNumbers
		ifTrue: [ textMorph withLineNumbers ]
		ifFalse: [ textMorph withoutLineNumbers ].
	aPresentation withAnnotation
		ifTrue: [ textMorph withAnnotation ]
		ifFalse: [ textMorph withoutAnnotation ].
	aPresentation withColumns 
		ifTrue: [ textMorph withColumns ]
		ifFalse: [ textMorph withoutColumns ].
	textMorph wrapped: aPresentation wrapped.
	textMorph tabWidth: aPresentation tabWidth.
	aPresentation textSegments do: [:each | 
			textMorph addSegment: each ].
	(aPresentation textSegments anySatisfy: [ :ts | ts icon notNil ])
		ifTrue: [ textMorph withTextSegmentIcons ]
		ifFalse: [ textMorph withoutTextSegmentIcons ].
	textMorph textArea announcer
		when: RubTextChanged
		do: [ :ann | 
			aPresentation announcer
				suspendAllWhile: [ aPresentation text: textMorph text ] ].
	textMorph textArea announcer
		when: RubSelectionChanged
		do: [ :ann | 
			aPresentation announcer 
				suspendAllWhile: [ 
					aPresentation selectionInterval: textMorph selectionInterval.
					aPresentation selectedText: textMorph textArea selection ] ].
			
	textMorph textArea announcer
		when: RubTextAccepted
		do: [ :ann | aPresentation suspendAllWhile: [ aPresentation text: ann text ] ].
		
	self installActionsOnModel: textModel fromPresentation: aPresentation .
	self installKeystrokeActionsOnMorph: textMorph textArea fromPresentation: aPresentation.
	
	self registerEventsForPresentation: aPresentation.
	
	^ textMorph