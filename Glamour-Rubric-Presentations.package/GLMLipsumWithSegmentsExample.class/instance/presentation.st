accessing
presentation
	^ GLMCompositePresentation new
		with: [ :a | 
			| t seg |
			t := a text.
"			t withLineNumbers: true.
			t withAnnotation: true.
			t wrapped: true.
			t tabWidth: 40.
			t withColumns: true.
"			
			seg := RubUnderlinedSegmentMorph from: 1 to: 29.
			seg icon: (self iconProvider iconNamed: #smallErrorIcon).
			seg label: 'Please review'.
			seg iconBlock: [ :segment :event | segment delete ].
			t addTextSegment: seg.
			
			seg := RubPlainSegmentMorph from: 535 to: 659.
			seg color: (Color green alpha: 0.3).
			seg icon: (self iconProvider  iconNamed: #smallErrorIcon).
			seg label: 'Remove'.
			seg
				iconBlock: [ :segment :event | 
					segment textArea selectFrom: segment firstIndex to: segment lastIndex.
					segment textArea replaceSelectionWith: ''.
					segment delete ].
			t addTextSegment: seg.
			seg := RubUnderlinedSegmentMorph from: 1000 to: 1030.
			seg icon: (self iconProvider iconNamed: #smallHelpIcon).
			seg label: 'Print it'.
			seg iconBlock: [ :segment :event | Transcript show: segment getText ].
			t addTextSegment: seg]