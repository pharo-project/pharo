accessing
presentation
	^ GLMCompositePresentation new
		with: [ :a | 
			| t seg |
			t := a text.
			seg := RubPlainSegmentMorph from: 400 to: 746.
			seg color: (Color green alpha: 0.3).
			seg setBalloonText: 'This sentence should be removed. Click on it to remove it'.
			seg
				when: #mouseDown
				send: #value
				to: [ 
					seg textArea selectFrom: seg firstIndex to: seg lastIndex.
					seg textArea replaceSelectionWith: ''.
					seg delete ].
			t addTextSegment: seg ]