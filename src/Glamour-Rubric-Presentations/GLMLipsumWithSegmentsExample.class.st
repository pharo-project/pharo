"
self new open
"
Class {
	#name : #GLMLipsumWithSegmentsExample,
	#superclass : #GLMRubricExample,
	#category : #'Glamour-Rubric-Presentations-Examples'
}

{ #category : #accessing }
GLMLipsumWithSegmentsExample >> presentation [
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
			seg icon: (self iconNamed: #smallErrorIcon).
			seg label: 'Please review'.
			seg iconBlock: [ :segment :event | segment delete ].
			t addTextSegment: seg.
			
			seg := RubPlainSegmentMorph from: 535 to: 659.
			seg color: (Color green alpha: 0.3).
			seg icon: (self iconNamed: #smallErrorIcon).
			seg label: 'Remove'.
			seg
				iconBlock: [ :segment :event | 
					segment textArea selectFrom: segment firstIndex to: segment lastIndex.
					segment textArea replaceSelectionWith: ''.
					segment delete ].
			t addTextSegment: seg.
			seg := RubUnderlinedSegmentMorph from: 1000 to: 1030.
			seg icon: (self iconNamed: #smallHelpIcon).
			seg label: 'Print it'.
			seg iconBlock: [ :segment :event | self trace: segment getText ].
			t addTextSegment: seg]
]
